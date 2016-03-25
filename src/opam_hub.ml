open Printf
open Cmdliner
open Lwt.Infix

let return = Lwt.return
module Smap = CCMap.Make(String)
module Client = OpamClient.SafeAPI

let opam_state =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  ref (OpamState.load_state "list")

exception Found of OpamPackage.t
let latest_or_installed pkgs =
  try
    OpamPackage.Set.fold (fun pkg latest ->
      if OpamState.is_package_installed !opam_state pkg
      then raise_notrace (Found pkg)
      else
        match OpamPackage.(Version.compare (version latest) (version pkg)) with
        | 1 -> latest
        | -1
        | 0 -> pkg
        | _ -> assert false
    ) pkgs (OpamPackage.Set.choose pkgs)
  with Found p -> p

let github_repos =
  List.fold_left (fun m name ->
    let pkg = OpamPackage.Name.of_string name in
    let pkgs = OpamState.find_packages_by_name !opam_state pkg in
    if OpamPackage.Set.is_empty pkgs then
      failwith @@ sprintf "Name %s doesn't match any packages" name
    else
      let pkg = latest_or_installed pkgs in
      Smap.add name pkg m
  ) Smap.empty

let dev_repo_url opam =
  opam
  |> OpamFile.OPAM.dev_repo
  |> CCOpt.map (fun r -> Uri.of_string (OpamTypesBase.string_of_pin_option r))

let maintainers =
  ["opium" ; "cohttp" ; "ppx_deriving"]
  |> github_repos
  |> Smap.iter (fun name pkg ->
    let opam_file = OpamState.opam !opam_state pkg in
    opam_file
    |> dev_repo_url
    |> CCOpt.iter (fun repo_url ->
      printf "Pkg %s has repo: %s\n" name (Uri.to_string repo_url)
    )
  )

let () =
  let opam = OpamFile.OPAM.read (OpamFilename.of_string "opam") in
  begin match OpamFile.OPAM.dev_repo opam with
  | None -> print_endline "no dev repo"
  | Some dev_repo ->
    let uri = dev_repo |> OpamTypesBase.string_of_pin_option |> Uri.of_string in
    begin match Uri.host uri with
    | Some "github.com" ->
      printf "github: %s\n" (Uri.to_string uri)
    | Some _ -> print_endline "not github..."
    | None -> print_endline "no dev repo"
    end
  end;
