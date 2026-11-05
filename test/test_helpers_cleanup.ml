let test_with_temp_dir_cleanup () =
  let check_cleanup raise_in_callback =
    let created = ref None in
    Fun.protect
      ~finally:(fun () -> Option.iter Test_helpers.rm_tree !created)
      (fun () ->
        let exception Callback_failed in
        let outcome =
          try
            let result =
              Test_helpers.with_temp_dir (fun dir ->
                  created := Some dir;
                  let nested = Filename.concat dir "nested" in
                  Unix.mkdir nested 0o700;
                  let deeper = Filename.concat nested "deeper" in
                  Unix.mkdir deeper 0o700;
                  let oc = open_out (Filename.concat deeper "file") in
                  close_out oc;
                  if raise_in_callback then raise Callback_failed;
                  42)
            in
            Alcotest.(check int) "callback result preserved" 42 result;
            false
          with Callback_failed -> true
        in
        Alcotest.(check bool)
          "callback exception preserved" raise_in_callback outcome;
        match !created with
        | Some dir -> not (Sys.file_exists dir)
        | None -> Alcotest.fail "callback was not called")
  in
  let success_cleaned = check_cleanup false in
  let exception_cleaned = check_cleanup true in
  Alcotest.(check (pair bool bool))
    "nested trees removed after success and exception" (true, true)
    (success_cleaned, exception_cleaned)

let test_rm_tree_symlinks () =
  Test_helpers.with_temp_dir (fun outside ->
      let sentinel = Filename.concat outside "keep" in
      let oc = open_out sentinel in
      close_out oc;
      Test_helpers.with_temp_dir (fun dir ->
          Unix.symlink outside (Filename.concat dir "directory-link");
          Unix.symlink
            (Filename.concat outside "missing")
            (Filename.concat dir "dangling-link");
          Fun.protect
            ~finally:(fun () ->
              List.iter
                (fun name ->
                  try Unix.unlink (Filename.concat dir name) with _ -> ())
                [ "directory-link"; "dangling-link" ];
              Test_helpers.rm_tree dir)
            (fun () ->
              Test_helpers.rm_tree dir;
              Alcotest.(check (pair bool bool))
                "tree removed and symlink target preserved" (false, true)
                (Sys.file_exists dir, Sys.file_exists sentinel))))

let suite =
  [
    Alcotest.test_case
      "with_temp_dir removes nested dirs on success and on exception" `Quick
      test_with_temp_dir_cleanup;
    Alcotest.test_case "rm_tree removes symlinks without following them" `Quick
      test_rm_tree_symlinks;
  ]
