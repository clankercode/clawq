external available_c : unit -> bool = "caml_landlock_available"
external create_ruleset_c : int -> int = "caml_landlock_create_ruleset"
external add_rule_path_c : int -> string -> int -> unit = "caml_landlock_add_rule_path"
external restrict_self_c : int -> unit = "caml_landlock_restrict_self"
external close_fd_c : int -> unit = "caml_landlock_close_fd"

let available () = try available_c () with _ -> false

(* ABI v1 access flags *)
let _access_fs_execute     = 1
let access_fs_write_file   = 2
let access_fs_read_file    = 4
let access_fs_read_dir     = 8
let _access_fs_remove_dir  = 16
let _access_fs_remove_file = 32
let _access_fs_make_char   = 64
let access_fs_make_dir     = 128
let access_fs_make_reg     = 256
let _access_fs_make_sock   = 512
let _access_fs_make_fifo   = 1024
let _access_fs_make_block  = 2048
let _access_fs_make_sym    = 4096

let access_fs_read = access_fs_read_file lor access_fs_read_dir
let access_fs_write =
  access_fs_write_file lor access_fs_make_dir lor access_fs_make_reg
let access_fs_rw = access_fs_read lor access_fs_write

(* Full ABI v1 mask: all 13 bits *)
let access_fs_all = (1 lsl 13) - 1

let add_path_safe ruleset_fd path access =
  try
    if Sys.file_exists path then
      add_rule_path_c ruleset_fd path access
    else
      Logs.debug (fun m -> m "Landlock: skipping non-existent path %s" path)
  with exn ->
    Logs.warn (fun m ->
        m "Landlock: failed to add rule for %s: %s" path
          (Printexc.to_string exn))

let sandbox_workspace ~(config : Runtime_config.t) =
  if not (available ()) then begin
    Logs.warn (fun m ->
        m "Landlock not available on this kernel; sandbox not activated");
  end
  else
    try
      let workspace = Runtime_config.effective_workspace config in
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      let clawq_dir = Filename.concat home ".clawq" in
      let ruleset_fd = create_ruleset_c access_fs_all in
      (try
        (* Workspace: read-write *)
        add_path_safe ruleset_fd workspace access_fs_rw;
        (* Config dir: read-write *)
        add_path_safe ruleset_fd clawq_dir access_fs_rw;
        (* System paths: read-only *)
        List.iter
          (fun p -> add_path_safe ruleset_fd p access_fs_read)
          [ "/usr"; "/lib"; "/lib64"; "/etc/ssl"; "/etc/resolv.conf";
            "/proc/self" ];
        (* Tmp: read-write *)
        add_path_safe ruleset_fd "/tmp" access_fs_rw;
        (* Extra read paths from config *)
        List.iter
          (fun p ->
            let expanded = Runtime_config.expand_home p in
            add_path_safe ruleset_fd expanded access_fs_read)
          config.security.landlock_extra_read_paths;
        (* restrict_self_c closes the fd internally *)
        restrict_self_c ruleset_fd
      with exn ->
        (* Close fd if restrict_self wasn't reached *)
        (try close_fd_c ruleset_fd with _ -> ());
        raise exn);
      Logs.info (fun m -> m "Landlock sandbox activated")
    with exn ->
      Logs.err (fun m ->
          m "Landlock sandbox activation failed: %s (continuing without sandbox)"
            (Printexc.to_string exn))
