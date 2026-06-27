(** Tests for [Room_workspace] — safe persistent room workspace paths. *)

(* -- helpers --------------------------------------------------------------- *)

let contains_sub s sub =
  let sl = String.length s in
  let subl = String.length sub in
  let rec go i =
    if i + subl > sl then false
    else if String.sub s i subl = sub then true
    else go (i + 1)
  in
  subl = 0 || go 0

let with_temp_dir f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base (Printf.sprintf "cw_test_%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with _ -> ());
  Fun.protect
    ~finally:(fun () ->
      (* best-effort cleanup: remove our temp dir *)
      let rec rmrf p =
        (try
           Array.iter
             (fun entry ->
               let fp = Filename.concat p entry in
               match (Unix.lstat fp).Unix.st_kind with
               | Unix.S_DIR -> rmrf fp
               | _ -> Unix.unlink fp)
             (Sys.readdir p)
         with _ -> ());
        try Unix.rmdir p with _ -> ()
      in
      rmrf dir)
    (fun () -> f dir)

(* Override Dot_dir.path for testing by setting CLAWQ_HOME *)
let with_clawq_home dir f =
  let old = try Some (Sys.getenv "CLAWQ_HOME") with Not_found -> None in
  Unix.putenv "CLAWQ_HOME" dir;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some v -> Unix.putenv "CLAWQ_HOME" v
      | None -> Unix.putenv "CLAWQ_HOME" "")
    f

let touch_mtime path mtime = Unix.utimes path mtime mtime

(* -- tests ----------------------------------------------------------------- *)

(** Deterministic: same room_id always gives same path. *)
let test_deterministic () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p1 = Room_workspace.workspace_path "slack:C123:U456" in
          let p2 = Room_workspace.workspace_path "slack:C123:U456" in
          Alcotest.(check string) "same path" p1 p2))

(** Path contains a slug and a hash separated by hyphen. *)
let test_slug_format () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "MyRoom-Alpha" in
          let basename = Filename.basename p in
          (* Must contain a hyphen separating slug from hash *)
          Alcotest.(check bool) "has hyphen" true (contains_sub basename "-");
          (* Slug portion is lowercased *)
          Alcotest.(check bool)
            "slug lowercase" true
            (contains_sub basename "myroom")))

(** Hash suffix is at least 12 hex characters. *)
let test_hash_length () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "test-room" in
          let basename = Filename.basename p in
          (* Find last hyphen — everything after it is the hash *)
          let last_hyphen =
            let r = ref (-1) in
            String.iteri (fun i c -> if c = '-' then r := i) basename;
            !r
          in
          let hash =
            String.sub basename (last_hyphen + 1)
              (String.length basename - last_hyphen - 1)
          in
          Alcotest.(check bool)
            "hash >= 12 chars" true
            (String.length hash >= 12);
          (* Must be hex *)
          let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
          Alcotest.(check bool) "hash is hex" true (String.for_all is_hex hash)))

(** Different room IDs produce different paths (no collision). *)
let test_no_collision () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p1 = Room_workspace.workspace_path "room-alpha" in
          let p2 = Room_workspace.workspace_path "room-beta" in
          Alcotest.(check bool) "different paths" true (p1 <> p2)))

let test_routine_workspace_isolated_from_room_workspace () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let room_path = Room_workspace.workspace_path "C01" in
          let routine_path =
            Room_workspace.routine_workspace_path ~create:true ~profile_id:"vip"
              ~routine_id:"daily-briefing"
          in
          Alcotest.(check bool)
            "routine path differs from room/task path" true
            (routine_path <> room_path);
          Alcotest.(check bool)
            "routine path is under routines root" true
            (String.starts_with
               ~prefix:(Room_workspace.routines_root ())
               routine_path)))

(** Workspace directory is created under rooms root. *)
let test_creates_dir () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "create-test" in
          Alcotest.(check bool) "dir exists" true (Sys.file_exists p);
          Alcotest.(check bool) "is directory" true (Sys.is_directory p)))

(** Path is under ~/.clawq/workspace/rooms/. *)
let test_under_rooms_root () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "some-room" in
          let rooms = Room_workspace.rooms_root () in
          Alcotest.(check bool)
            "under rooms root" true
            (String.length p > String.length rooms
            && String.sub p 0 (String.length rooms) = rooms)))

(** Unicode room ID is handled safely (no crash, no control chars in path). *)
let test_unicode_room_id () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "房间-会议室" in
          (* Should not crash; slug strips non-ascii, hash is deterministic *)
          Alcotest.(check bool) "dir exists" true (Sys.file_exists p)))

(** Empty-ish room IDs still produce a valid path (fallback slug). *)
let test_empty_room_id () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let p = Room_workspace.workspace_path "!!!###" in
          (* Slug strips special chars, falls back to "room" *)
          let basename = Filename.basename p in
          Alcotest.(check bool)
            "starts with room" true
            (String.length basename >= 4 && String.sub basename 0 4 = "room")))

(** Long room ID is truncated, not rejected. *)
let test_long_room_id () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let long_id = String.make 500 'a' in
          let p = Room_workspace.workspace_path long_id in
          let basename = Filename.basename p in
          (* Basename should be manageable length *)
          Alcotest.(check bool)
            "basename not absurd" true
            (String.length basename < 100)))

(* -- override validation tests -------------------------------------------- *)

(** Empty override is rejected. *)
let test_override_empty () =
  match Room_workspace.validate_override "" with
  | Ok _ -> Alcotest.fail "expected error for empty path"
  | Error _ -> ()

(** Traversal (..) is rejected. *)
let test_override_traversal () =
  match Room_workspace.validate_override "/tmp/foo/../bar" with
  | Ok _ -> Alcotest.fail "expected error for traversal"
  | Error e ->
      Alcotest.(check string)
        "traversal error" "workspace_dir contains path traversal (..)"
        (Room_workspace.validation_error_to_string e)

(** Double traversal is rejected. *)
let test_override_traversal_deep () =
  match Room_workspace.validate_override "/tmp/a/b/../../etc/passwd" with
  | Ok _ -> Alcotest.fail "expected error for deep traversal"
  | Error _ -> ()

(** Control characters are rejected. *)
let test_override_control_chars () =
  match Room_workspace.validate_override "/tmp/foo\x01bar" with
  | Ok _ -> Alcotest.fail "expected error for control chars"
  | Error e ->
      Alcotest.(check string)
        "control chars error"
        "workspace_dir contains control characters (0x00-0x1F or 0x7F)"
        (Room_workspace.validation_error_to_string e)

(** Null byte is rejected. *)
let test_override_null_byte () =
  match Room_workspace.validate_override "/tmp/foo\000bar" with
  | Ok _ -> Alcotest.fail "expected error for null byte"
  | Error _ -> ()

(** DEL char (0x7F) is rejected. *)
let test_override_del_char () =
  match Room_workspace.validate_override "/tmp/foo\x7Fbar" with
  | Ok _ -> Alcotest.fail "expected error for DEL char"
  | Error _ -> ()

(** Overly long component (>255 bytes) is rejected. *)
let test_override_long_component () =
  let long_name = String.make 300 'a' in
  let path = "/tmp/" ^ long_name in
  match Room_workspace.validate_override path with
  | Ok _ -> Alcotest.fail "expected error for long component"
  | Error e ->
      Alcotest.(check string)
        "long component error"
        "workspace_dir name component exceeds maximum length (255 bytes)"
        (Room_workspace.validation_error_to_string e)

(** Symlink escape: a symlink pointing outside rooms root is rejected. *)
let test_override_symlink_escape () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let rooms = Room_workspace.rooms_root () in
          Room_workspace.ensure_dir rooms;
          (* Create a symlink in rooms pointing to /tmp *)
          let link_path = Filename.concat rooms "escape-link" in
          (try Unix.symlink "/tmp" link_path with _ -> ());
          match Room_workspace.validate_override link_path with
          | Ok _ -> Alcotest.fail "expected error for symlink escape"
          | Error _ -> ()))

(** Path under rooms root is accepted when the symlink stays contained. *)
let test_override_under_rooms () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let rooms = Room_workspace.rooms_root () in
          Room_workspace.ensure_dir rooms;
          let target = Filename.concat rooms "valid-room" in
          Room_workspace.ensure_dir target;
          match Room_workspace.validate_override target with
          | Error e ->
              Alcotest.failf "expected ok, got: %s"
                (Room_workspace.validation_error_to_string e)
          | Ok resolved ->
              Alcotest.(check bool)
                "resolved under rooms" true
                (String.length resolved >= String.length rooms)))

(** Path under allowed extra path is accepted. *)
let test_override_under_extra_allowed () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let cwd = Sys.getcwd () in
          let sub = Filename.concat cwd "workspace-test-subdir" in
          Room_workspace.ensure_dir sub;
          match
            Room_workspace.validate_override ~extra_allowed_paths:[ cwd ] sub
          with
          | Error e ->
              Alcotest.failf "expected ok under extra allowed path, got: %s"
                (Room_workspace.validation_error_to_string e)
          | Ok resolved ->
              Alcotest.(check bool)
                "resolved under extra allowed" true
                (String.length resolved >= String.length cwd)))

(** Path not under rooms root or allowed paths is rejected. *)
let test_override_not_allowed () =
  match Room_workspace.validate_override "/tmp/not-allowed-here" with
  | Ok _ -> Alcotest.fail "expected error for path outside containment"
  | Error _ -> ()

(** Path under an unallowed directory is rejected even if extra_allowed_paths is
    provided with a different path. *)
let test_override_extra_allowed_rejects_other () =
  match
    Room_workspace.validate_override ~extra_allowed_paths:[ "/home/user" ]
      "/tmp/not-allowed"
  with
  | Ok _ -> Alcotest.fail "expected error for path outside extra_allowed_paths"
  | Error _ -> ()

(** resolve_workspace with no override uses deterministic path. *)
let test_resolve_no_override () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          match Room_workspace.resolve_workspace ~is_admin:true "my-room" with
          | Error e ->
              Alcotest.failf "expected ok, got: %s"
                (Room_workspace.validation_error_to_string e)
          | Ok p ->
              let expected = Room_workspace.workspace_path "my-room" in
              Alcotest.(check string) "same as workspace_path" expected p))

(** resolve_workspace with valid override uses the override (admin). *)
let test_resolve_valid_override () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let rooms = Room_workspace.rooms_root () in
          Room_workspace.ensure_dir rooms;
          let target = Filename.concat rooms "my-override" in
          Room_workspace.ensure_dir target;
          match
            Room_workspace.resolve_workspace ~is_admin:true
              ~workspace_dir:target "x"
          with
          | Error e ->
              Alcotest.failf "expected ok, got: %s"
                (Room_workspace.validation_error_to_string e)
          | Ok p ->
              Alcotest.(check bool)
                "resolved path is under rooms" true
                (String.length p >= String.length rooms)))

(** resolve_workspace with invalid override returns error. *)
let test_resolve_invalid_override () =
  match
    Room_workspace.resolve_workspace ~is_admin:true ~workspace_dir:"/etc/passwd"
      "x"
  with
  | Ok _ -> Alcotest.fail "expected error for invalid override"
  | Error _ -> ()

(** resolve_workspace with workspace_dir override requires admin. *)
let test_override_admin_only () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let rooms = Room_workspace.rooms_root () in
          Room_workspace.ensure_dir rooms;
          let target = Filename.concat rooms "admin-override" in
          Room_workspace.ensure_dir target;
          match
            Room_workspace.resolve_workspace ~is_admin:false
              ~workspace_dir:target "x"
          with
          | Ok _ -> Alcotest.fail "expected admin-only error for override"
          | Error e ->
              Alcotest.(check string)
                "admin-only error"
                "workspace_dir override requires admin privileges"
                (Room_workspace.validation_error_to_string e)))

(** resolve_workspace without override works for any caller. *)
let test_resolve_non_admin_no_override () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          match Room_workspace.resolve_workspace ~is_admin:false "my-room" with
          | Error e ->
              Alcotest.failf
                "expected ok for non-admin without override, got: %s"
                (Room_workspace.validation_error_to_string e)
          | Ok _ -> ()))

(** Unicode homoglyphs in path are handled (no crash). *)
let test_override_unicode_homoglyphs () =
  (* These are valid unicode but not control chars — should pass control check
     but likely fail containment *)
  match Room_workspace.validate_override "/tmp/рареnt" with
  | Ok _ -> Alcotest.fail "expected containment error"
  | Error _ -> ()

let test_gc_purges_only_expired_paths () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let now = 1_000_000.0 in
          let old_path = Room_workspace.workspace_path "old-room" in
          let recent_path = Room_workspace.workspace_path "recent-room" in
          touch_mtime old_path (now -. 7200.0);
          touch_mtime recent_path (now -. 60.0);
          let result = Room_workspace.gc ~now ~retention_seconds:3600.0 () in
          Alcotest.(check bool)
            "old path purged" false (Sys.file_exists old_path);
          Alcotest.(check bool)
            "recent path preserved" true
            (Sys.file_exists recent_path);
          Alcotest.(check int) "one purged" 1 (List.length result.purged);
          Alcotest.(check int) "one preserved" 1 (List.length result.preserved)))

let test_gc_preserves_active_reference () =
  with_temp_dir (fun home ->
      with_clawq_home home (fun () ->
          let now = 1_000_000.0 in
          let active_path = Room_workspace.workspace_path "active-room" in
          let stale_path = Room_workspace.workspace_path "stale-room" in
          touch_mtime active_path (now -. 7200.0);
          touch_mtime stale_path (now -. 7200.0);
          let result =
            Room_workspace.gc ~now ~retention_seconds:3600.0
              ~protected_paths:[ active_path ] ()
          in
          Alcotest.(check bool)
            "active path preserved" true
            (Sys.file_exists active_path);
          Alcotest.(check bool)
            "stale path purged" false
            (Sys.file_exists stale_path);
          Alcotest.(check int) "one purged" 1 (List.length result.purged);
          Alcotest.(check int) "one preserved" 1 (List.length result.preserved)))

let test_room_ids_for_reference_derives_channel_room () =
  Alcotest.(check (list string))
    "room ids"
    [ "slack:C1:U1"; "C1"; "slack:C1" ]
    (Room_workspace.room_ids_for_reference ~channel_id:"C1" "slack:C1:U1")

(* -- suite ----------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "deterministic" `Quick test_deterministic;
    Alcotest.test_case "slug format" `Quick test_slug_format;
    Alcotest.test_case "hash length >= 12 hex" `Quick test_hash_length;
    Alcotest.test_case "no collision" `Quick test_no_collision;
    Alcotest.test_case "routine workspace isolated from room workspace" `Quick
      test_routine_workspace_isolated_from_room_workspace;
    Alcotest.test_case "creates dir" `Quick test_creates_dir;
    Alcotest.test_case "under rooms root" `Quick test_under_rooms_root;
    Alcotest.test_case "unicode room id" `Quick test_unicode_room_id;
    Alcotest.test_case "empty room id" `Quick test_empty_room_id;
    Alcotest.test_case "long room id" `Quick test_long_room_id;
    Alcotest.test_case "override empty" `Quick test_override_empty;
    Alcotest.test_case "override traversal" `Quick test_override_traversal;
    Alcotest.test_case "override deep traversal" `Quick
      test_override_traversal_deep;
    Alcotest.test_case "override control chars" `Quick
      test_override_control_chars;
    Alcotest.test_case "override null byte" `Quick test_override_null_byte;
    Alcotest.test_case "override DEL char" `Quick test_override_del_char;
    Alcotest.test_case "override long component" `Quick
      test_override_long_component;
    Alcotest.test_case "override symlink escape" `Quick
      test_override_symlink_escape;
    Alcotest.test_case "override under rooms" `Quick test_override_under_rooms;
    Alcotest.test_case "override under extra allowed" `Quick
      test_override_under_extra_allowed;
    Alcotest.test_case "override not allowed" `Quick test_override_not_allowed;
    Alcotest.test_case "override extra allowed rejects other" `Quick
      test_override_extra_allowed_rejects_other;
    Alcotest.test_case "resolve no override" `Quick test_resolve_no_override;
    Alcotest.test_case "resolve valid override" `Quick
      test_resolve_valid_override;
    Alcotest.test_case "override invalid" `Quick test_resolve_invalid_override;
    Alcotest.test_case "override admin only" `Quick test_override_admin_only;
    Alcotest.test_case "non-admin no override ok" `Quick
      test_resolve_non_admin_no_override;
    Alcotest.test_case "override unicode homoglyphs" `Quick
      test_override_unicode_homoglyphs;
    Alcotest.test_case "gc purges only expired paths" `Quick
      test_gc_purges_only_expired_paths;
    Alcotest.test_case "gc preserves active reference" `Quick
      test_gc_preserves_active_reference;
    Alcotest.test_case "reference derives channel room" `Quick
      test_room_ids_for_reference_derives_channel_room;
  ]
