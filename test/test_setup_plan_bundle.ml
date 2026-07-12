(** Tests for setup-owned bundle attach/detach (P19.M1.E1.T004). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Setup_plan_bundle.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let test_attach_first_time () =
  with_db @@ fun db ->
  match
    Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"bundle-gh"
      ~feature_id:"route:1" ~setup_plan_id:"plan_1" ()
  with
  | Ok (Attached { linkage; first_time = true }) ->
      Alcotest.(check string) "room" "room-a" linkage.room_id;
      Alcotest.(check string) "bundle" "bundle-gh" linkage.bundle_id;
      Alcotest.(check string) "owner" "setup" linkage.provenance.owner;
      Alcotest.(check string) "status" "attached" linkage.status
  | Ok _ -> Alcotest.fail "expected first attach"
  | Error e -> Alcotest.fail e

let test_attach_idempotent_reuse () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f1" ());
  match
    Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
      ~feature_id:"f1" ()
  with
  | Ok (Reused { linkage }) ->
      Alcotest.(check string) "status" "attached" linkage.status
  | Ok (Attached _) -> Alcotest.fail "expected reuse"
  | Error e -> Alcotest.fail e

let test_detach_last_feature () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f1" ());
  match
    Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
      ~bundle_id:"b" ~feature_id:"f1" ()
  with
  | Ok (Detached { linkage }) ->
      Alcotest.(check string) "detached" "detached" linkage.status;
      Alcotest.(check bool)
        "no longer setup-owned" false
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-a" ~bundle_id:"b"
           ())
  | Ok _ -> Alcotest.fail "expected full detach"
  | Error e -> Alcotest.fail e

let test_detach_preserves_when_other_features () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f1" ());
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f2" ());
  match
    Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
      ~bundle_id:"b" ~feature_id:"f1" ()
  with
  | Ok (Still_attached { remaining_features; _ }) ->
      Alcotest.(check int) "remaining" 1 remaining_features;
      Alcotest.(check bool)
        "still setup-owned" true
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-a" ~bundle_id:"b"
           ())
  | Ok _ -> Alcotest.fail "expected still_attached"
  | Error e -> Alcotest.fail e

let test_last_removal_reports_detached_after_previous_removal () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f1" ());
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f2" ());
  ignore
    (Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
       ~bundle_id:"b" ~feature_id:"f1" ());
  match
    Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
      ~bundle_id:"b" ~feature_id:"f2" ()
  with
  | Ok (Detached _) ->
      Alcotest.(check bool)
        "no setup-owned linkage remains" false
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-a" ~bundle_id:"b"
           ())
  | Ok _ -> Alcotest.fail "last feature removal must detach the linkage"
  | Error e -> Alcotest.fail e

let test_unrelated_bundle_untouched () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"setup-b"
       ~feature_id:"f1" ());
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"other-b"
       ~feature_id:"f2" ());
  ignore
    (Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
       ~bundle_id:"setup-b" ~feature_id:"f1" ());
  Alcotest.(check bool)
    "other still attached" true
    (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-a" ~bundle_id:"other-b"
       ())

let test_inspect_room () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b1"
       ~feature_id:"f1" ());
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b2"
       ~feature_id:"f2" ());
  ignore
    (Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
       ~bundle_id:"b1" ~feature_id:"f1" ());
  let all = Setup_plan_bundle.inspect_room ~db ~room_id:"room-a" () in
  let attached = Setup_plan_bundle.list_attached ~db ~room_id:"room-a" () in
  Alcotest.(check int) "history has 2" 2 (List.length all);
  Alcotest.(check int) "attached has 1" 1 (List.length attached)

let test_remove_missing () =
  with_db @@ fun db ->
  match
    Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
      ~bundle_id:"b" ~feature_id:"missing" ()
  with
  | Ok Not_found -> ()
  | Ok _ -> Alcotest.fail "expected not_found"
  | Error e -> Alcotest.fail e

let test_reattach_after_detach () =
  with_db @@ fun db ->
  ignore
    (Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
       ~feature_id:"f1" ());
  ignore
    (Setup_plan_bundle.remove_managed_feature ~db ~room_id:"room-a"
       ~bundle_id:"b" ~feature_id:"f1" ());
  match
    Setup_plan_bundle.attach ~db ~room_id:"room-a" ~bundle_id:"b"
      ~feature_id:"f1" ()
  with
  | Ok (Attached { first_time = false; linkage }) ->
      Alcotest.(check string) "reattached" "attached" linkage.status
  | Ok (Reused _) -> () (* also acceptable *)
  | Ok (Attached { first_time = true; _ }) ->
      Alcotest.fail "should not be first_time after reattach"
  | Error e -> Alcotest.fail e

let suite =
  [
    ("attach first time", `Quick, test_attach_first_time);
    ("attach idempotent reuse", `Quick, test_attach_idempotent_reuse);
    ("detach last feature", `Quick, test_detach_last_feature);
    ( "detach preserves when other features",
      `Quick,
      test_detach_preserves_when_other_features );
    ( "last removal detaches after prior removal",
      `Quick,
      test_last_removal_reports_detached_after_previous_removal );
    ("unrelated bundle untouched", `Quick, test_unrelated_bundle_untouched);
    ("inspect room", `Quick, test_inspect_room);
    ("remove missing", `Quick, test_remove_missing);
    ("reattach after detach", `Quick, test_reattach_after_detach);
  ]
