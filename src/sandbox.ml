type backend = Firejail | Bubblewrap | None
type t = { backend : backend; workspace : string }

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

let create ~workspace () =
  let backend = detect () in
  { backend; workspace }

let bind_if_exists path =
  if Sys.file_exists path then
    " --bind " ^ Filename.quote path ^ " " ^ Filename.quote path
  else ""

let wrap_command t cmd =
  match t.backend with
  | None -> cmd
  | Firejail ->
      Printf.sprintf
        "firejail --private=%s --net=none --quiet --noprofile -- /bin/sh -c %s"
        (Filename.quote t.workspace)
        (Filename.quote cmd)
  | Bubblewrap ->
      let extra_binds =
        bind_if_exists "/lib" ^ bind_if_exists "/lib64" ^ bind_if_exists "/bin"
        ^ bind_if_exists "/etc"
      in
      let dev_binds =
        " --dev /dev" ^ bind_if_exists "/dev/null"
        ^ bind_if_exists "/dev/urandom"
      in
      Printf.sprintf
        "bwrap --ro-bind /usr /usr --unshare-all --bind %s %s%s%s --tmpfs /tmp \
         --die-with-parent -- /bin/sh -c %s"
        (Filename.quote t.workspace)
        (Filename.quote t.workspace)
        extra_binds dev_binds (Filename.quote cmd)
