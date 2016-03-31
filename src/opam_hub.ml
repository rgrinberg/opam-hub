open Printf
open Cmdliner
open Lwt.Infix

let return = Lwt.return
module Smap = CCMap.Make(String)
module Client = OpamClient.SafeAPI

type error =
  | Infer_repo_error of string
  | Not_github of Uri.t
  | No_dev_repo of OpamFile.OPAM.t
  | Package_not_found of string
  | Issues_prs_enabled
exception Hub_error of error

let () =
  (* TODO Add the rest *)
  Printexc.register_printer (function
    | Hub_error (Not_github u) ->
      Some (sprintf "Not github uri: '%s'" (Uri.to_string u))
    | _ -> None )

let raise_hub e = raise (Hub_error e)

type github_repo =
  { user: string
  ; repo: string }

let find_maintainer github_repo =
  github_repo.user (* XXX doesn't work when user is an org *)

let github_repo_of_uri =
  let re =
    let open Re in
    let chunk = group (rep1 (alt [wordc ; char '-'])) in
    [ chunk ; str "/" ; chunk ]
    |> seq
    |> compile in
  fun uri ->
    match Uri.host uri with
    | Some "github.com" ->
      begin match Re.exec re (Uri.path uri) with
      | s -> { user=Re.get s 1 ; repo=Re.get s 2 }
      | exception Not_found -> raise_hub (Not_github uri)
      end
    | Some _
    | None -> raise_hub (Not_github uri)

let browse url =
  let cmd =
    if OpamGlobals.os () = OpamGlobals.Darwin
    then "open"
    else "xdg-open" in
  OpamSystem.command [cmd ; Uri.to_string url]

let last_char = function
  | "" -> None
  | s -> Some (s.[String.length s - 1])

let append_uri uri ~path =
  let uri_path = Uri.path uri in
  let new_path =
    match last_char uri_path, last_char path with
    | Some '/', Some '/' -> uri_path ^ (Stringext.drop path 1)
    | Some _, Some '/' -> uri_path ^ path
    | Some _, Some _ -> uri_path ^ "/" ^ path
    | None, Some _ -> path
    | _, None -> invalid_arg "append_url: second arg can't be empty" in
  Uri.with_path uri new_path

let browse_issues github_url =
  browse (append_uri github_url ~path:"issues")

let browse_prs github_url =
  browse (append_uri github_url ~path:"pulls")

let opam_state =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  ref (OpamState.load_state "list")

let latest_or_installed pkgs =
  let module M = struct exception Found of OpamPackage.t end in
  try
    OpamPackage.Set.fold (fun pkg latest ->
      if OpamState.is_package_installed !opam_state pkg
      then raise_notrace (M.Found pkg)
      else
        match OpamPackage.(Version.compare (version latest) (version pkg)) with
        | 1 -> latest
        | -1
        | 0 -> pkg
        | _ -> assert false
    ) pkgs (OpamPackage.Set.choose pkgs)
  with M.Found p -> p

let find_opam_packages =
  List.fold_left (fun m name ->
    let pkg = OpamPackage.Name.of_string name in
    let pkgs = OpamState.find_packages_by_name !opam_state pkg in
    if OpamPackage.Set.is_empty pkgs then
      failwith @@ sprintf "Name %s doesn't match any package" name
    else
      let pkg = latest_or_installed pkgs in
      Smap.add name pkg m
  ) Smap.empty

let find_opam_package name =
  let pkg = OpamPackage.Name.of_string name in
  let pkgs = OpamState.find_packages_by_name !opam_state pkg in
  if OpamPackage.Set.is_empty pkgs then
    failwith @@ sprintf "Name %s doesn't match any package" name
  else latest_or_installed pkgs

let dev_repo_url opam =
  match OpamFile.OPAM.dev_repo opam with
  | None -> raise_hub (No_dev_repo opam)
  | Some r -> Uri.of_string (OpamTypesBase.string_of_pin_option r)

let github_base_url = (* strips the .git *)
  let re =
    let open Re in
    [ group (rep1 any) ; str ".git" ; eos ]
    |> seq |> compile in
  fun uri ->
    match Re.exec re (Uri.path uri) with
    | g -> Uri.with_path uri (Re.get g 1)
    | exception Not_found -> uri

let dev_repo_github_url opam =
  opam |> dev_repo_url |> github_base_url

(* TODO also try and guess this from the remote *)
let guess_current_repo () =
  if Sys.file_exists "./opam" then
    "./opam"
    |> OpamFilename.of_string
    |> OpamFile.OPAM.read
  else raise_hub (Infer_repo_error "local opam file doesn't exist")

let packages_of_args = function
  | [] -> [ guess_current_repo () ]
  | pkgs ->
    pkgs |> List.map (fun p ->
      p
      |> find_opam_package
      |> OpamState.opam !opam_state)

let package =
  Arg.(value & pos 0 (some string) None & info [] ~doc:"")

let packages =
  Arg.(value & pos_all string [] & info [] ~doc:"TODO")

let maintainers pkgs =
  pkgs
  |> packages_of_args
  |> List.map dev_repo_url
  |> List.iter (fun uri ->
    let repo = github_repo_of_uri uri in
    printf "%s\n" (find_maintainer repo))

let browse pkgs issues prs =
  let f =
    match issues, prs with
    | false, false -> fun x -> x
    | false, true -> append_uri ~path:"pulls"
    | true, false -> append_uri ~path:"issues"
    | true, true -> raise_hub Issues_prs_enabled in
  pkgs
  |> packages_of_args
  |> List.map dev_repo_github_url
  |> List.map f
  |> List.iter browse

let maintainers =
  let open Term in
  pure maintainers $ packages,
  info "maintainers" ~doc:"display maintainer names"

let browse =
  let open Term in
  let issues = Arg.(value & flag & info ["issues" ; "i"]) in
  let prs = Arg.(value & flag & info ["prs" ; "p"]) in
  pure browse $ packages $ issues $ prs,
  info "browse" ~doc:"browse github page"

let default_cmd =
  let doc = "win @ opam + github" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
    `S "DESCRIPTION";
    `P "Win at opam + github";
    `S "AUTHORIZATION OPTIONS";
    `P "use ocaml-github's jar thing";
    `S "COMMON OPTIONS";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `P "$(b,--json) Show JSON responses.";
    `P "$(b,--pretty) pretty print JSON responses.";
    `S "BUGS";
    `P "Report them online at <http://github.com/rgrinberg/opam-hub>."] in
  Term.info "opam-hub" ~doc ~man

let () =
  match Term.eval_choice default_cmd [maintainers ; browse] with
  | `Error _ -> exit 1
  | _ -> exit 0
