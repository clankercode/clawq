let with_temp_env f =
  Test_helpers.with_temp_home (fun home ->
      let workspace = Filename.concat home "workspace" in
      Unix.mkdir workspace 0o755;
      let write name content =
        let path = Filename.concat workspace name in
        Workspace_scaffold.ensure_dir (Filename.dirname path);
        let oc = open_out path in
        output_string oc content;
        close_out oc
      in
      write "EGO.md" "I am the ego.";
      write "AGENTS.md" "Agent protocol.";
      write "USER.md" "User info.";
      f ~workspace ~write)

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let test_backup_creates_version () =
  with_temp_env (fun ~workspace ~write:_ ->
      let name = "test-v1" in
      match Workspace_version.backup ~workspace ~name with
      | Error e -> Alcotest.failf "backup failed: %s" e
      | Ok files ->
          Alcotest.(check bool) "has files" true (List.length files >= 3);
          let vdir = Filename.concat (Workspace_version.versions_dir ()) name in
          Alcotest.(check bool) "version dir exists" true (Sys.file_exists vdir);
          let meta_path = Filename.concat vdir "_meta.json" in
          Alcotest.(check bool)
            "_meta.json exists" true
            (Sys.file_exists meta_path);
          let ego = read_file (Filename.concat vdir "EGO.md") in
          Alcotest.(check string) "EGO.md content" "I am the ego." ego)

let test_backup_fails_if_exists () =
  with_temp_env (fun ~workspace ~write:_ ->
      let name = "dup-test" in
      (match Workspace_version.backup ~workspace ~name with
      | Error e -> Alcotest.failf "first backup failed: %s" e
      | Ok _ -> ());
      match Workspace_version.backup ~workspace ~name with
      | Ok _ -> Alcotest.fail "expected error for duplicate name"
      | Error e ->
          Alcotest.(check bool)
            "mentions exists" true
            (String_util.contains e "already exists"))

let test_restore_copies_files_back () =
  with_temp_env (fun ~workspace ~write:_ ->
      let name = "restore-test" in
      (match Workspace_version.backup ~workspace ~name with
      | Error e -> Alcotest.failf "backup failed: %s" e
      | Ok _ -> ());
      let ego_path = Filename.concat workspace "EGO.md" in
      let oc = open_out ego_path in
      output_string oc "MODIFIED";
      close_out oc;
      (match Workspace_version.restore ~workspace ~name with
      | Error e -> Alcotest.failf "restore failed: %s" e
      | Ok _ -> ());
      let ego = read_file ego_path in
      Alcotest.(check string) "EGO.md restored" "I am the ego." ego)

let test_list_versions_sorted () =
  with_temp_env (fun ~workspace ~write:_ ->
      (match Workspace_version.backup ~workspace ~name:"aaa-first" with
      | Error e -> Alcotest.failf "first backup failed: %s" e
      | Ok _ -> ());
      Unix.sleepf 0.01;
      (match Workspace_version.backup ~workspace ~name:"zzz-second" with
      | Error e -> Alcotest.failf "second backup failed: %s" e
      | Ok _ -> ());
      let versions = Workspace_version.list_versions () in
      Alcotest.(check int) "two versions" 2 (List.length versions);
      let names = List.map fst versions in
      Alcotest.(check (list string))
        "newest first"
        [ "zzz-second"; "aaa-first" ]
        names)

let test_delete_removes_version () =
  with_temp_env (fun ~workspace ~write:_ ->
      let name = "del-test" in
      (match Workspace_version.backup ~workspace ~name with
      | Error e -> Alcotest.failf "backup failed: %s" e
      | Ok _ -> ());
      (match Workspace_version.delete ~name with
      | Error e -> Alcotest.failf "delete failed: %s" e
      | Ok () -> ());
      let vdir = Filename.concat (Workspace_version.versions_dir ()) name in
      Alcotest.(check bool) "version gone" false (Sys.file_exists vdir))

let test_name_validation () =
  Alcotest.(check bool)
    "empty" true
    (Result.is_error (Workspace_version.backup ~workspace:"/tmp" ~name:""));
  Alcotest.(check bool)
    "spaces" true
    (Result.is_error
       (Workspace_version.backup ~workspace:"/tmp" ~name:"has space"));
  Alcotest.(check bool)
    "slashes" true
    (Result.is_error (Workspace_version.backup ~workspace:"/tmp" ~name:"a/b"));
  let long = String.make 129 'a' in
  Alcotest.(check bool)
    "too long" true
    (Result.is_error (Workspace_version.backup ~workspace:"/tmp" ~name:long));
  Alcotest.(check bool)
    "valid" true
    (Result.is_ok (Workspace_version.validate_name "my-backup.2026-03-15_v1"))

let test_auto_backup_name_format () =
  let name = Workspace_version.auto_backup_name () in
  Alcotest.(check bool)
    "starts with pre-reset-" true
    (String.length name > 11 && String.sub name 0 10 = "pre-reset-");
  Alcotest.(check bool)
    "valid name" true
    (Result.is_ok (Workspace_version.validate_name name))

let test_restore_nonexistent () =
  with_temp_env (fun ~workspace ~write:_ ->
      match Workspace_version.restore ~workspace ~name:"nonexistent" with
      | Ok _ -> Alcotest.fail "expected error"
      | Error e ->
          Alcotest.(check bool)
            "mentions not exist" true
            (String_util.contains e "does not exist"))

let test_backup_with_subdirs () =
  with_temp_env (fun ~workspace ~write ->
      write "memory/2026-03-15.md" "Today's notes.";
      let name = "subdir-test" in
      match Workspace_version.backup ~workspace ~name with
      | Error e -> Alcotest.failf "backup failed: %s" e
      | Ok files ->
          Alcotest.(check bool)
            "includes subdir file" true
            (List.mem "memory/2026-03-15.md" files);
          let vdir = Filename.concat (Workspace_version.versions_dir ()) name in
          let content =
            read_file (Filename.concat vdir "memory/2026-03-15.md")
          in
          Alcotest.(check string) "subdir content" "Today's notes." content)

let test_delete_nonexistent () =
  with_temp_env (fun ~workspace:_ ~write:_ ->
      match Workspace_version.delete ~name:"nonexistent" with
      | Ok () -> Alcotest.fail "expected error"
      | Error e ->
          Alcotest.(check bool)
            "mentions not exist" true
            (String_util.contains e "does not exist"))

let suite =
  [
    Alcotest.test_case "backup creates version" `Quick
      test_backup_creates_version;
    Alcotest.test_case "backup fails if exists" `Quick
      test_backup_fails_if_exists;
    Alcotest.test_case "restore copies files back" `Quick
      test_restore_copies_files_back;
    Alcotest.test_case "list versions sorted" `Quick test_list_versions_sorted;
    Alcotest.test_case "delete removes version" `Quick
      test_delete_removes_version;
    Alcotest.test_case "name validation" `Quick test_name_validation;
    Alcotest.test_case "auto_backup_name format" `Quick
      test_auto_backup_name_format;
    Alcotest.test_case "restore nonexistent fails" `Quick
      test_restore_nonexistent;
    Alcotest.test_case "backup with subdirs" `Quick test_backup_with_subdirs;
    Alcotest.test_case "delete nonexistent fails" `Quick test_delete_nonexistent;
  ]
