let make_db () =
  let db = Sqlite3.db_open ":memory:" in
  Held_items.init_db db;
  db

let with_temp_db f =
  let db = Sqlite3.db_open ":memory:" in
  Held_items.init_db db;
  Fun.protect (fun () -> f db) ~finally:(fun () -> ignore (Sqlite3.db_close db))

let test_init_db () = with_temp_db (fun db -> Held_items.init_db db)

let test_save_and_get () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"test-feat"
          ~description:"A test feature" ~plan_json:"{\"steps\": []}" ~layer:1 ()
      in
      Alcotest.(check bool) "id > 0" true (id > 0);
      match Held_items.get ~db ~id with
      | None -> Alcotest.fail "expected item to exist"
      | Some item ->
          Alcotest.(check string) "name" "test-feat" item.feature_name;
          Alcotest.(check string) "desc" "A test feature" item.description;
          Alcotest.(check string) "plan" "{\"steps\": []}" item.plan_json;
          Alcotest.(check int) "layer" 1 item.layer;
          Alcotest.(check string) "status" "pending" item.status;
          Alcotest.(check (option string)) "requestor" None item.requestor_id;
          Alcotest.(check (option string)) "channel" None item.channel)

let test_save_with_optional_fields () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"feat2" ~description:"desc2"
          ~plan_json:"{}" ~layer:6 ~requestor_id:"user42" ~channel:"telegram"
          ~session_key:"tg:user42" ()
      in
      match Held_items.get ~db ~id with
      | None -> Alcotest.fail "expected item"
      | Some item ->
          Alcotest.(check (option string))
            "requestor" (Some "user42") item.requestor_id;
          Alcotest.(check (option string))
            "channel" (Some "telegram") item.channel;
          Alcotest.(check (option string))
            "session_key" (Some "tg:user42") item.session_key)

let test_list_pending () =
  with_temp_db (fun db ->
      ignore
        (Held_items.save ~db ~feature_name:"a" ~description:"d1" ~plan_json:"{}"
           ~layer:1 ());
      ignore
        (Held_items.save ~db ~feature_name:"b" ~description:"d2" ~plan_json:"{}"
           ~layer:2 ());
      let items = Held_items.list_items ~db ~status:"pending" () in
      Alcotest.(check int) "two pending items" 2 (List.length items))

let test_list_all () =
  with_temp_db (fun db ->
      let id1 =
        Held_items.save ~db ~feature_name:"a" ~description:"d1" ~plan_json:"{}"
          ~layer:1 ()
      in
      ignore
        (Held_items.save ~db ~feature_name:"b" ~description:"d2" ~plan_json:"{}"
           ~layer:2 ());
      ignore (Held_items.review ~db ~id:id1 ~action:"approved" ());
      let pending = Held_items.list_items ~db ~status:"pending" () in
      Alcotest.(check int) "one pending" 1 (List.length pending);
      let all = Held_items.list_items ~db ~status:"all" () in
      Alcotest.(check int) "two total" 2 (List.length all))

let test_approve () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"f" ~description:"d" ~plan_json:"{}"
          ~layer:6 ()
      in
      let ok =
        Held_items.review ~db ~id ~action:"approved" ~reviewed_by:"admin1"
          ~notes:"looks good" ()
      in
      Alcotest.(check bool) "approve succeeds" true ok;
      match Held_items.get ~db ~id with
      | None -> Alcotest.fail "expected item"
      | Some item ->
          Alcotest.(check string) "status" "approved" item.status;
          Alcotest.(check (option string))
            "reviewed_by" (Some "admin1") item.reviewed_by;
          Alcotest.(check (option string))
            "notes" (Some "looks good") item.review_notes;
          Alcotest.(check bool)
            "reviewed_at set" true
            (Option.is_some item.reviewed_at))

let test_reject () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"f" ~description:"d" ~plan_json:"{}"
          ~layer:6 ()
      in
      let ok =
        Held_items.review ~db ~id ~action:"rejected" ~notes:"not needed" ()
      in
      Alcotest.(check bool) "reject succeeds" true ok;
      match Held_items.get ~db ~id with
      | None -> Alcotest.fail "expected item"
      | Some item -> Alcotest.(check string) "status" "rejected" item.status)

let test_double_review_fails () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"f" ~description:"d" ~plan_json:"{}"
          ~layer:6 ()
      in
      ignore (Held_items.review ~db ~id ~action:"approved" ());
      let ok2 = Held_items.review ~db ~id ~action:"rejected" () in
      Alcotest.(check bool) "second review fails" false ok2)

let test_delete () =
  with_temp_db (fun db ->
      let id =
        Held_items.save ~db ~feature_name:"f" ~description:"d" ~plan_json:"{}"
          ~layer:1 ()
      in
      let ok = Held_items.delete ~db ~id in
      Alcotest.(check bool) "delete succeeds" true ok;
      Alcotest.(check (option reject))
        "gone" None
        (Held_items.get ~db ~id |> Option.map (fun _ -> ())))

let test_delete_nonexistent () =
  with_temp_db (fun db ->
      let ok = Held_items.delete ~db ~id:9999 in
      Alcotest.(check bool) "delete nonexistent fails" false ok)

let test_get_nonexistent () =
  with_temp_db (fun db ->
      Alcotest.(check bool)
        "nonexistent returns None" true
        (Held_items.get ~db ~id:9999 = None))

let test_slash_command_parsing () =
  let check input expected_str =
    let result = Slash_commands.handle input in
    let got = Test_slash_commands.result_to_string result in
    Alcotest.(check string) input expected_str got
  in
  check "/held-items" "HeldItems(List false)";
  check "/held-items list" "HeldItems(List false)";
  check "/held-items list --all" "HeldItems(List true)";
  check "/held-items view 42" "HeldItems(Show 42)";
  check "/held-items show 7" "HeldItems(Show 7)"

let test_slash_approve_is_admin_gated () =
  let result = Slash_commands.handle "/held-items approve 5" in
  match result with
  | Slash_commands.AdminRequired (Slash_commands.HeldItems (HeldItemsApprove 5))
    ->
      ()
  | _ ->
      Alcotest.fail
        ("expected AdminRequired(HeldItems(Approve 5)), got "
        ^ Test_slash_commands.result_to_string result)

let test_slash_reject_is_admin_gated () =
  let result = Slash_commands.handle "/held-items reject 3 not useful" in
  match result with
  | Slash_commands.AdminRequired
      (Slash_commands.HeldItems (HeldItemsReject (3, Some "not useful"))) ->
      ()
  | _ ->
      Alcotest.fail
        ("expected AdminRequired(HeldItems(Reject 3 ...)), got "
        ^ Test_slash_commands.result_to_string result)

let suite =
  [
    Alcotest.test_case "init_db idempotent" `Quick test_init_db;
    Alcotest.test_case "save and get" `Quick test_save_and_get;
    Alcotest.test_case "save with optional fields" `Quick
      test_save_with_optional_fields;
    Alcotest.test_case "list pending" `Quick test_list_pending;
    Alcotest.test_case "list all" `Quick test_list_all;
    Alcotest.test_case "approve" `Quick test_approve;
    Alcotest.test_case "reject" `Quick test_reject;
    Alcotest.test_case "double review fails" `Quick test_double_review_fails;
    Alcotest.test_case "delete" `Quick test_delete;
    Alcotest.test_case "delete nonexistent" `Quick test_delete_nonexistent;
    Alcotest.test_case "get nonexistent" `Quick test_get_nonexistent;
    Alcotest.test_case "slash command parsing" `Quick test_slash_command_parsing;
    Alcotest.test_case "approve is admin-gated" `Quick
      test_slash_approve_is_admin_gated;
    Alcotest.test_case "reject is admin-gated" `Quick
      test_slash_reject_is_admin_gated;
  ]
