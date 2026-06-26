(* Tests for Sandbox module *)

let mk_sandbox ?(extra_allowed_paths = []) ?(isolate_filesystem = true) ~backend
    ~workspace () =
  { Sandbox.backend; workspace; extra_allowed_paths; isolate_filesystem }

(* --- backend type tests --- *)

let test_none_backend () =
  let sb = mk_sandbox ~backend:Sandbox.None ~workspace:"/tmp" () in
  Alcotest.(check string)
    "none wraps identity" "ls"
    (Sandbox.wrap_command sb "ls")

let test_firejail_wrap () =
  let sb = mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/workspace" () in
  let wrapped = Sandbox.wrap_command sb "echo hello" in
  Alcotest.(check bool)
    "contains firejail" true
    (Test_helpers.string_contains wrapped "firejail");
  Alcotest.(check bool)
    "contains workspace" true
    (Test_helpers.string_contains wrapped "/workspace");
  Alcotest.(check bool)
    "contains command" true
    (Test_helpers.string_contains wrapped "echo hello")

let test_bubblewrap_wrap () =
  let sb = mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/workspace" () in
  let wrapped = Sandbox.wrap_command sb "echo hello" in
  Alcotest.(check bool)
    "contains bwrap" true
    (Test_helpers.string_contains wrapped "bwrap");
  Alcotest.(check bool)
    "contains workspace" true
    (Test_helpers.string_contains wrapped "/workspace");
  Alcotest.(check bool)
    "contains command" true
    (Test_helpers.string_contains wrapped "echo hello")

(* --- is_available tests --- *)

let test_none_always_available () =
  Alcotest.(check bool)
    "None always available" true
    (Sandbox.is_available Sandbox.None)

(* --- detect tests --- *)

let test_detect_returns_backend () =
  let b = Sandbox.detect () in
  (* Should return one of Firejail, Bubblewrap, or None *)
  let is_valid =
    match b with Sandbox.Firejail | Sandbox.Bubblewrap | Sandbox.None -> true
  in
  Alcotest.(check bool) "detect returns valid backend" true is_valid

(* --- create tests --- *)

let test_create_workspace () =
  let sb =
    Sandbox.create ~workspace:"/test/workspace" ~extra_allowed_paths:[]
      ~workspace_only:true ()
  in
  Alcotest.(check string) "workspace" "/test/workspace" sb.workspace

let test_create_sets_backend () =
  let sb =
    Sandbox.create ~workspace:"/tmp" ~extra_allowed_paths:[]
      ~workspace_only:true ()
  in
  let is_valid =
    match sb.backend with
    | Sandbox.Firejail | Sandbox.Bubblewrap | Sandbox.None -> true
  in
  Alcotest.(check bool) "backend set" true is_valid

let test_create_tracks_extra_allowed_paths () =
  let sb =
    Sandbox.create ~workspace:"/workspace"
      ~extra_allowed_paths:[ "~/src"; "/workspace"; "~/src" ]
      ~workspace_only:true ()
  in
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Alcotest.(check (list string))
    "extra paths expanded and deduped"
    [ Filename.concat home "src" ]
    sb.extra_allowed_paths;
  Alcotest.(check bool)
    "workspace_only sets isolation" true sb.isolate_filesystem

(* --- bind_if_exists tests --- *)

let test_bind_if_exists_existing () =
  let result = Sandbox.bind_if_exists "/tmp" in
  Alcotest.(check bool)
    "contains /tmp" true
    (Test_helpers.string_contains result "/tmp" || result = "")

let test_bind_if_exists_nonexistent () =
  let result = Sandbox.bind_if_exists "/nonexistent_path_abc_xyz_123" in
  Alcotest.(check string) "empty for nonexistent" "" result

(* --- wrap_command edge cases --- *)

let test_wrap_empty_command () =
  let sb = mk_sandbox ~backend:Sandbox.None ~workspace:"/tmp" () in
  Alcotest.(check string) "empty command" "" (Sandbox.wrap_command sb "")

let test_wrap_firejail_net_none () =
  let sb = mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/tmp" () in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "net=none" true
    (Test_helpers.string_contains wrapped "--net=none")

let test_wrap_firejail_quiet () =
  let sb = mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/tmp" () in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "quiet" true
    (Test_helpers.string_contains wrapped "--quiet")

let test_wrap_bubblewrap_ro_bind () =
  let sb = mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/tmp" () in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "ro-bind /usr" true
    (Test_helpers.string_contains wrapped "--ro-bind /usr /usr")

let test_wrap_bubblewrap_unshare () =
  let sb = mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/tmp" () in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "unshare-all" true
    (Test_helpers.string_contains wrapped "--unshare-all")

let test_wrap_bubblewrap_die_with_parent () =
  let sb = mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/tmp" () in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "die-with-parent" true
    (Test_helpers.string_contains wrapped "--die-with-parent")

let test_wrap_firejail_whitelists_extra_paths () =
  let sb =
    mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/tmp"
      ~extra_allowed_paths:[ "/etc/hosts" ] ()
  in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "contains whitelist" true
    (Test_helpers.string_contains wrapped "--whitelist='/etc/hosts'")

let test_wrap_bubblewrap_binds_extra_paths () =
  let sb =
    mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/tmp"
      ~extra_allowed_paths:[ "/etc/hosts" ] ()
  in
  let wrapped = Sandbox.wrap_command sb "test" in
  Alcotest.(check bool)
    "contains extra bind" true
    (Test_helpers.string_contains wrapped "--bind '/etc/hosts' '/etc/hosts'")

let test_wrap_bubblewrap_ro_binds_user_bin_dirs () =
  Test_helpers.with_temp_home (fun home ->
      let local_bin = Filename.concat home ".local/bin" in
      let opam_bin = Filename.concat home ".opam/agent/bin" in
      Test_helpers.rm_tree local_bin;
      Test_helpers.rm_tree opam_bin;
      Unix.mkdir (Filename.concat home ".local") 0o755;
      Unix.mkdir local_bin 0o755;
      Unix.mkdir (Filename.concat home ".opam") 0o755;
      Unix.mkdir (Filename.concat home ".opam/agent") 0o755;
      Unix.mkdir opam_bin 0o755;
      let sb = mk_sandbox ~backend:Sandbox.Bubblewrap ~workspace:"/tmp" () in
      let wrapped = Sandbox.wrap_command sb "test" in
      Alcotest.(check bool)
        "ro-binds ~/.local/bin" true
        (Test_helpers.string_contains wrapped
           (Printf.sprintf "--ro-bind %s %s" (Filename.quote local_bin)
              (Filename.quote local_bin)));
      Alcotest.(check bool)
        "ro-binds ~/.opam/*/bin" true
        (Test_helpers.string_contains wrapped
           (Printf.sprintf "--ro-bind %s %s" (Filename.quote opam_bin)
              (Filename.quote opam_bin))))

let test_wrap_firejail_whitelists_user_bin_dirs () =
  Test_helpers.with_temp_home (fun home ->
      let pnpm_home = Filename.concat home ".local/share/pnpm" in
      let pnpm_bin = Filename.concat pnpm_home "bin" in
      Test_helpers.rm_tree pnpm_home;
      Unix.mkdir (Filename.concat home ".local") 0o755;
      Unix.mkdir (Filename.concat home ".local/share") 0o755;
      Unix.mkdir pnpm_home 0o755;
      Unix.mkdir pnpm_bin 0o755;
      let sb = mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/tmp" () in
      let wrapped = Sandbox.wrap_command sb "test" in
      Alcotest.(check bool)
        "whitelists pnpm home" true
        (Test_helpers.string_contains wrapped
           ("--whitelist=" ^ Filename.quote pnpm_home));
      Alcotest.(check bool)
        "whitelists pnpm bin" true
        (Test_helpers.string_contains wrapped
           ("--whitelist=" ^ Filename.quote pnpm_bin)))

let test_wrap_command_skips_fs_isolation_when_disabled () =
  let sb =
    mk_sandbox ~backend:Sandbox.Firejail ~workspace:"/tmp"
      ~isolate_filesystem:false ()
  in
  Alcotest.(check string)
    "returns raw command" "echo hello"
    (Sandbox.wrap_command sb "echo hello")

let suite =
  [
    Alcotest.test_case "none backend identity" `Quick test_none_backend;
    Alcotest.test_case "firejail wrap" `Quick test_firejail_wrap;
    Alcotest.test_case "bubblewrap wrap" `Quick test_bubblewrap_wrap;
    Alcotest.test_case "none always available" `Quick test_none_always_available;
    Alcotest.test_case "detect returns backend" `Quick
      test_detect_returns_backend;
    Alcotest.test_case "create workspace" `Quick test_create_workspace;
    Alcotest.test_case "create sets backend" `Quick test_create_sets_backend;
    Alcotest.test_case "create tracks extra paths" `Quick
      test_create_tracks_extra_allowed_paths;
    Alcotest.test_case "bind_if_exists existing" `Quick
      test_bind_if_exists_existing;
    Alcotest.test_case "bind_if_exists nonexistent" `Quick
      test_bind_if_exists_nonexistent;
    Alcotest.test_case "wrap empty command" `Quick test_wrap_empty_command;
    Alcotest.test_case "firejail net=none" `Quick test_wrap_firejail_net_none;
    Alcotest.test_case "firejail quiet" `Quick test_wrap_firejail_quiet;
    Alcotest.test_case "bubblewrap ro-bind" `Quick test_wrap_bubblewrap_ro_bind;
    Alcotest.test_case "bubblewrap unshare" `Quick test_wrap_bubblewrap_unshare;
    Alcotest.test_case "bubblewrap die-with-parent" `Quick
      test_wrap_bubblewrap_die_with_parent;
    Alcotest.test_case "firejail whitelists extra paths" `Quick
      test_wrap_firejail_whitelists_extra_paths;
    Alcotest.test_case "bubblewrap binds extra paths" `Quick
      test_wrap_bubblewrap_binds_extra_paths;
    Alcotest.test_case "bubblewrap ro-binds user bin dirs" `Quick
      test_wrap_bubblewrap_ro_binds_user_bin_dirs;
    Alcotest.test_case "firejail whitelists user bin dirs" `Quick
      test_wrap_firejail_whitelists_user_bin_dirs;
    Alcotest.test_case "disabled fs isolation returns raw command" `Quick
      test_wrap_command_skips_fs_isolation_when_disabled;
  ]
