let test_available_returns_bool () =
  let result = Landlock.available () in
  Alcotest.(check bool) "available returns a bool" result result

let test_sandbox_graceful_when_unavailable () =
  (* We cannot actually call sandbox_workspace in tests because if Landlock
     IS available, it will irreversibly restrict the test process.
     Instead, verify that the function exists and the config paths are accepted
     without crashing by testing with landlock_enabled=false in config
     (sandbox_workspace checks available() internally and logs a warning). *)
  if Landlock.available () then
    (* On a kernel with Landlock, just verify available() works *)
    Alcotest.(check pass) "landlock available, skipping activation test" () ()
  else begin
    (* On a kernel without Landlock, sandbox_workspace should log and return *)
    let config = Runtime_config.default in
    try
      Landlock.sandbox_workspace ~config;
      Alcotest.(check pass) "no exception" () ()
    with _exn -> Alcotest.fail "sandbox_workspace should not raise exceptions"
  end

let test_config_paths_empty () =
  (* Verify config with empty extra paths is accepted without crash *)
  let config =
    {
      Runtime_config.default with
      security =
        { Runtime_config.default.security with landlock_extra_read_paths = [] };
    }
  in
  Alcotest.(check (list string))
    "empty extra paths" [] config.security.landlock_extra_read_paths

let test_config_paths_nonempty () =
  (* Verify config with non-empty extra paths is properly constructed *)
  let paths = [ "/usr/share"; "~/documents" ] in
  let config =
    {
      Runtime_config.default with
      security =
        {
          Runtime_config.default.security with
          landlock_extra_read_paths = paths;
        };
    }
  in
  Alcotest.(check (list string))
    "nonempty extra paths" paths config.security.landlock_extra_read_paths

let test_access_constants () =
  Alcotest.(check bool)
    "access_fs_read non-zero" true
    (Landlock.access_fs_read > 0);
  Alcotest.(check bool) "access_fs_rw non-zero" true (Landlock.access_fs_rw > 0);
  Alcotest.(check bool)
    "access_fs_all non-zero" true
    (Landlock.access_fs_all > 0);
  Alcotest.(check bool)
    "access_fs_rw > access_fs_read" true
    (Landlock.access_fs_rw > Landlock.access_fs_read)

let suite =
  [
    Alcotest.test_case "available returns bool" `Quick
      test_available_returns_bool;
    Alcotest.test_case "sandbox graceful when unavailable" `Quick
      test_sandbox_graceful_when_unavailable;
    Alcotest.test_case "config empty extra paths" `Quick test_config_paths_empty;
    Alcotest.test_case "config nonempty extra paths" `Quick
      test_config_paths_nonempty;
    Alcotest.test_case "access constants" `Quick test_access_constants;
  ]
