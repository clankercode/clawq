let write_bytes path n =
  let oc = open_out path in
  output_string oc (String.make n 'x');
  close_out oc

let file_size path =
  try (Unix.stat path).Unix.st_size with Unix.Unix_error _ -> 0

let file_exists path = Sys.file_exists path

let test_no_rotation_under_threshold () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      write_bytes log_path 500;
      let config : Runtime_config.log_config =
        { max_size_mb = 1; max_files = 3; debug_http = false }
      in
      let rotated = Log_rotation.maybe_rotate ~log_path ~config in
      Alcotest.(check bool) "should not rotate" false rotated;
      Alcotest.(check bool) "original still exists" true (file_exists log_path);
      Alcotest.(check bool)
        ".1 should not exist" false
        (file_exists (log_path ^ ".1")))

let test_rotation_over_threshold () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      (* 1 MB = 1048576 bytes; write 1.1 MB *)
      write_bytes log_path 1_100_000;
      let config : Runtime_config.log_config =
        { max_size_mb = 1; max_files = 3; debug_http = false }
      in
      (* Save original stdout/stderr and restore after test *)
      let saved_stdout = Unix.dup Unix.stdout in
      let saved_stderr = Unix.dup Unix.stderr in
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved_stdout Unix.stdout;
          Unix.dup2 saved_stderr Unix.stderr;
          Unix.close saved_stdout;
          Unix.close saved_stderr)
        (fun () ->
          let rotated = Log_rotation.maybe_rotate ~log_path ~config in
          Alcotest.(check bool) "should rotate" true rotated;
          Alcotest.(check bool)
            ".1 created" true
            (file_exists (log_path ^ ".1"));
          Alcotest.(check int)
            ".1 has old content" 1_100_000
            (file_size (log_path ^ ".1"));
          Alcotest.(check bool) "new log exists" true (file_exists log_path);
          Alcotest.(check bool)
            "new log is small" true
            (file_size log_path < 1024)))

let test_rotation_shifts_existing () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      write_bytes log_path 1_100_000;
      write_bytes (log_path ^ ".1") 100;
      write_bytes (log_path ^ ".2") 200;
      let config : Runtime_config.log_config =
        { max_size_mb = 1; max_files = 3; debug_http = false }
      in
      let saved_stdout = Unix.dup Unix.stdout in
      let saved_stderr = Unix.dup Unix.stderr in
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved_stdout Unix.stdout;
          Unix.dup2 saved_stderr Unix.stderr;
          Unix.close saved_stdout;
          Unix.close saved_stderr)
        (fun () ->
          let rotated = Log_rotation.maybe_rotate ~log_path ~config in
          Alcotest.(check bool) "should rotate" true rotated;
          Alcotest.(check int)
            ".1 is the old current" 1_100_000
            (file_size (log_path ^ ".1"));
          Alcotest.(check int) ".2 is old .1" 100 (file_size (log_path ^ ".2"));
          Alcotest.(check int) ".3 is old .2" 200 (file_size (log_path ^ ".3"));
          Alcotest.(check bool)
            "new log is small" true
            (file_size log_path < 1024)))

let test_rotation_deletes_oldest () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      write_bytes log_path 1_100_000;
      write_bytes (log_path ^ ".1") 100;
      write_bytes (log_path ^ ".2") 200;
      write_bytes (log_path ^ ".3") 300;
      let config : Runtime_config.log_config =
        { max_size_mb = 1; max_files = 3; debug_http = false }
      in
      let saved_stdout = Unix.dup Unix.stdout in
      let saved_stderr = Unix.dup Unix.stderr in
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved_stdout Unix.stdout;
          Unix.dup2 saved_stderr Unix.stderr;
          Unix.close saved_stdout;
          Unix.close saved_stderr)
        (fun () ->
          let rotated = Log_rotation.maybe_rotate ~log_path ~config in
          Alcotest.(check bool) "should rotate" true rotated;
          (* .3 was the oldest and should have been deleted before shift *)
          (* After shift: old .2 -> .3, old .1 -> .2, old current -> .1 *)
          Alcotest.(check int)
            ".1 is old current" 1_100_000
            (file_size (log_path ^ ".1"));
          Alcotest.(check int) ".2 is old .1" 100 (file_size (log_path ^ ".2"));
          Alcotest.(check int) ".3 is old .2" 200 (file_size (log_path ^ ".3"));
          Alcotest.(check bool) "no .4" false (file_exists (log_path ^ ".4"))))

let test_disabled_with_zero_size () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      write_bytes log_path 1_100_000;
      let config : Runtime_config.log_config =
        { max_size_mb = 0; max_files = 3; debug_http = false }
      in
      let rotated = Log_rotation.maybe_rotate ~log_path ~config in
      Alcotest.(check bool) "should not rotate when disabled" false rotated)

let test_missing_log_file () =
  Test_helpers.with_temp_dir (fun dir ->
      let log_path = Filename.concat dir "daemon.log" in
      let config : Runtime_config.log_config =
        { max_size_mb = 1; max_files = 3; debug_http = false }
      in
      let rotated = Log_rotation.maybe_rotate ~log_path ~config in
      Alcotest.(check bool) "should not rotate missing file" false rotated)

let suite =
  [
    Alcotest.test_case "no rotation under threshold" `Quick
      test_no_rotation_under_threshold;
    Alcotest.test_case "rotation over threshold" `Quick
      test_rotation_over_threshold;
    Alcotest.test_case "rotation shifts existing files" `Quick
      test_rotation_shifts_existing;
    Alcotest.test_case "rotation deletes oldest" `Quick
      test_rotation_deletes_oldest;
    Alcotest.test_case "disabled with zero size" `Quick
      test_disabled_with_zero_size;
    Alcotest.test_case "missing log file" `Quick test_missing_log_file;
  ]
