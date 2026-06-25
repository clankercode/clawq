type backend = Firejail | Bubblewrap | None

type t = {
  backend : backend;
  workspace : string;
  extra_allowed_paths : string list;
  isolate_filesystem : bool;
}

let backend_to_string = function
  | Firejail -> "firejail"
  | Bubblewrap -> "bubblewrap"
  | None -> "none"

let is_available b =
  let cmd =
    match b with
    | Firejail -> "which firejail 2>/dev/null"
    | Bubblewrap -> "which bwrap 2>/dev/null"
    | None -> ""
  in
  if cmd = "" then true else Sys.command cmd = 0

let detect () =
  if is_available Firejail then Firejail
  else if is_available Bubblewrap then Bubblewrap
  else None

let backend_of_policy policy =
  match String.lowercase_ascii (String.trim policy) with
  | "firejail" -> Firejail
  | "bubblewrap" | "bwrap" -> Bubblewrap
  | "none" -> None
  | _ -> detect ()

let create ?backend ~workspace ~extra_allowed_paths ~workspace_only () =
  let backend = match backend with Some b -> b | None -> detect () in
  let extra_allowed_paths =
    extra_allowed_paths
    |> List.map Runtime_config.expand_home
    |> List.filter (fun path -> path <> "" && path <> workspace)
    |> List.sort_uniq String.compare
  in
  {
    backend;
    workspace;
    extra_allowed_paths;
    isolate_filesystem = workspace_only;
  }

let bind_if_exists path =
  if Sys.file_exists path then
    " --bind " ^ Filename.quote path ^ " " ^ Filename.quote path
  else ""

let ro_bind_if_exists path =
  if Sys.file_exists path then
    " --ro-bind " ^ Filename.quote path ^ " " ^ Filename.quote path
  else ""

let whitelist_if_exists path =
  if Sys.file_exists path then " --whitelist=" ^ Filename.quote path else ""

let extra_binds t =
  String.concat "" (List.map bind_if_exists t.extra_allowed_paths)

let extra_whitelists t =
  String.concat "" (List.map whitelist_if_exists t.extra_allowed_paths)

let user_bin_ro_binds () =
  String.concat ""
    (List.map ro_bind_if_exists (Runtime_config.common_user_bin_dirs ()))

let user_bin_whitelists () =
  String.concat ""
    (List.map whitelist_if_exists (Runtime_config.common_user_bin_dirs ()))

let wrap_command t cmd =
  if not t.isolate_filesystem then cmd
  else
    match t.backend with
    | None -> cmd
    | Firejail ->
        Printf.sprintf
          "firejail --private=%s%s --net=none --quiet --noprofile -- /bin/sh \
           -c %s"
          (Filename.quote t.workspace)
          (extra_whitelists t ^ user_bin_whitelists ())
          (Filename.quote cmd)
    | Bubblewrap ->
        let extra_binds =
          bind_if_exists "/lib" ^ bind_if_exists "/lib64"
          ^ bind_if_exists "/bin" ^ ro_bind_if_exists "/etc" ^ extra_binds t
          ^ user_bin_ro_binds ()
        in
        let dev_binds =
          " --dev /dev" ^ bind_if_exists "/dev/null"
          ^ bind_if_exists "/dev/urandom"
        in
        Printf.sprintf
          "bwrap --ro-bind /usr /usr --unshare-all --bind %s %s%s%s --tmpfs \
           /tmp --die-with-parent -- /bin/sh -c %s"
          (Filename.quote t.workspace)
          (Filename.quote t.workspace)
          extra_binds dev_binds (Filename.quote cmd)
