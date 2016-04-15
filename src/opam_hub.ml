open Printf
open Cmdliner
open Lwt.Infix

let return = Lwt.return
module Client = OpamClient.SafeAPI
module Gh = Github_t

let (>>|) m f = Github.Monad.(>|=) m f

type error =
  | Infer_repo_error of string
  | Not_github of Uri.t
  | No_dev_repo of OpamFile.OPAM.t
  | Package_not_found of string
  | Fail of string
exception Hub_error of error

let opam_name p =
  p
  |> OpamFile.OPAM.name
  |> OpamPackage.Name.to_string

let string_of_error = function
  | Not_github u -> sprintf "Not github uri: '%s'" (Uri.to_string u)
  | No_dev_repo p -> sprintf "package %s has no dev-repo" (opam_name p)
  | Package_not_found p -> sprintf "package %s doesn't exist" p
  | Infer_repo_error s -> s
  | Fail f -> f

let () =
  Printexc.register_printer (function
    | Hub_error e -> Some (string_of_error e)
    | Github.Message (_, m) ->
      let json = Github_j.string_of_message m in
      Some (Yojson.Safe.prettify json)
    | _ -> None)

let raise_hub e = raise (Hub_error e)

type github_repo =
  { user: string
  ; repo: string }

let find_maintainer github_repo =
  github_repo.user (* XXX doesn't work when user is an org *)

let token =
  let token = lazy (Sys.getenv "GH_COOKIE") in
  fun () -> Github.Token.of_string (Lazy.force token)

let ask_fork { user ; repo } =
  let open Github.Monad in
  let token = token () in
  Github.Repo.fork ~token ~user ~repo () >>~ (fun a ->
    return a.Github_t.repository_clone_url
  ) |> run

module Git = struct
  let git_dir_path =
    let open Re in
    let re =
      [ char '/'
      ; group (rep1 any)
      ; opt (seq [str ".git"; opt (char '/')]) ]
      |> seq
      |> compile in
    fun s -> Re.get (Re.exec re s) 1

  let git args =
    Lwt_preemptive.detach (fun () ->
      OpamSystem.command ~verbose:true ("git" :: args)) ()

  let add_remote ~repo ~name ~url =
    git ["-C"; repo; "remote"; "add"; name; url]

  let clone ?dir ?branch ?(remotes=[]) url =
    let args =
      match branch with
      | Some b -> ["-b"; b]
      | None -> [] in
    let args = args @ [url] in
    let args =
      match dir with
      | None -> args
      | Some d -> args @ [d] in
    let args = "clone"::args in
    git args >>= fun () ->
    let repo =
      match dir with
      | None -> git_dir_path url
      | Some s -> s in
    remotes |> Lwt_list.iter_s (fun (name, url) -> add_remote ~repo ~name ~url)
end

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

let pr_pin_path base number =
  sprintf "%s#pull/%d/head" (Uri.to_string base) number

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

let package_of_arg = function
  | None -> guess_current_repo ()
  | Some p ->
    p
    |> find_opam_package
    |> OpamState.opam !opam_state

let package =
  Arg.(value & pos 0 (some string) None & info [] ~doc:"opam package name")

let packages =
  let doc = "opam packages. defaults to the package in the current dir" in
  Arg.(value & pos_all string [] & info [] ~doc)

let strict_packages =
  let doc = "opam packages" in
  Arg.(non_empty & pos_all string [] & info [] ~doc)

let pr_num =
  Arg.(required & pos 1 (some int) None & info [] ~doc:"pull request number")

let maintainers pkgs mentions unique sort =
  let nub l =
    l
    |> List.fold_left (fun acc a ->
      if List.mem a acc
      then acc
      else a::acc) []
    |> List.rev in
  let ms =
    pkgs
    |> packages_of_args
    |> List.map dev_repo_url
    |> List.map (fun uri -> uri |> github_repo_of_uri |> find_maintainer) in
  let ms = if unique then nub ms else ms in
  let ms = if sort then List.sort compare ms else ms in
  let ms = if mentions then List.map (sprintf "@%s") ms else ms in
  List.iter (printf "%s\n") ms

let browse pkgs issues prs =
  let f =
    match issues, prs with
    | false, false -> fun x -> x
    | false, true -> append_uri ~path:"pulls"
    | true, false -> append_uri ~path:"issues"
    | true, true -> raise_hub (Fail "can't set both --issues and --prs") in
  pkgs
  |> packages_of_args
  |> List.map dev_repo_github_url
  |> List.map f
  |> List.iter browse

let prs package =
  let package = package_of_arg package in
  let { user ; repo } =
    package
    |> dev_repo_github_url
    |> github_base_url
    |> github_repo_of_uri in
  Github.Issue.for_repo ~user ~repo ()
  |> Github.Stream.to_list
  >>| (fun issues ->
    issues
    |> List.filter (function
      | { Gh.issue_pull_request=Some _ } -> true
      | _ -> false)
    |> List.iter (fun i ->
      printf "%d: %s\n" i.Gh.issue_number i.Gh.issue_title))
  |> Github.Monad.run
  |> Lwt_main.run

let pin package pr =
  let package = package_of_arg package in
  let name = opam_name package in
  let base_url =
    package
    |> dev_repo_github_url
    |> github_base_url in
  let pin_path = pr_pin_path base_url pr in
  let pin_cmd =
    sprintf "opam pin add -k git %s '%s'" name pin_path in
  exit (Sys.command pin_cmd)

let clone packages git_name =
  let urls =
    packages
    |> packages_of_args
    |> List.map dev_repo_github_url in
  List.combine urls packages
  |> Lwt_list.iter_p (fun (u, p) ->
    let dir = if git_name then None else Some p in
    Git.clone ?dir (Uri.to_string u))
  |> Lwt_main.run

let fork packages git_name auto_remotes remote_name =
  let remote_name =
    match auto_remotes, remote_name with
    | true, Some _ ->
      raise_hub (Fail "Cannot use --auth-remote with --reomote")
    | false, Some n -> fun _ -> Some n
    | true, None -> fun p -> Some p
    | false, None -> fun _ -> None in
  let packages = packages_of_args packages in
  let repos =
    List.map (fun package ->
      package
      |> dev_repo_github_url
      |> github_base_url
      |> github_repo_of_uri) packages in
  List.combine repos packages
  |> Lwt_list.iter_p (fun (r, p) ->
    ask_fork r >>= fun s ->
    let opam_name = opam_name p in
    let dir = if git_name then None else Some opam_name in
    let remotes =
      match remote_name opam_name with
      | Some n -> [n, Uri.to_string (dev_repo_github_url p)]
      | None -> [] in
    Git.clone ?dir ~remotes s)
  |> Lwt_main.run

let maintainers =
  let open Term in
  let mentions = Arg.(value & flag & info ["mentions"; "m"]) in
  let unique = Arg.(value & flag & info ["unique"; "u"]) in
  let sort = Arg.(value & flag & info ["sort"; "s"]) in
  const maintainers $ packages $ mentions $ unique $ sort,
  info "maintainers" ~doc:"display maintainer names"

let browse =
  let open Term in
  let issues = Arg.(value & flag & info ["issues" ; "i"]) in
  let prs = Arg.(value & flag & info ["prs" ; "p"]) in
  const browse $ packages $ issues $ prs,
  info "browse" ~doc:"browse github page"

let prs =
  let open Term in
  const prs $ package,
  info "prs" ~doc:"show open pull requests"

let pin =
  let open Term in
  const pin $ package $ pr_num,
  info "pin" ~doc:"pin an opam package to a pull request"

let git_name = Arg.(value & flag & info ["git-name"; "g"])

let clone =
  let open Term in
  const clone $ strict_packages $ git_name,
  info "clone" ~doc:"clone an opam package"

let fork =
  let open Term in
  let auto_remotes = Arg.(value & flag & info ["auto-remote"; "a"]) in
  let remotes = Arg.(value & opt (some string) None & info ["remote"; "r"]) in
  const fork $ strict_packages $ git_name $ auto_remotes $ remotes,
  info "fork" ~doc:"fork and clone an opam package"

let default_cmd =
  Term.(ret (const (`Help (`Pager, None)))),
  let man = [
    `S "DESCRIPTION";
    `P "Opam + Github integration";
    `S "AUTHORIZATION OPTIONS";
    `P "use ocaml-github's jar thing";
    `S "COMMON OPTIONS";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `S "BUGS";
    `P "Report them online at <http://github.com/rgrinberg/opam-hub>."] in
  Term.info "opam-hub" ~man

let () =
  let cmds = [maintainers ; browse ; prs ; pin ; clone ; fork] in
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
