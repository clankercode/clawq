open Lwt.Syntax
open Test_slack_huddle_fixtures

let cfg =
  Config_loader.parse_config
    (Yojson.Safe.from_string {|{ "workspace": "/tmp/test" }|})

let with_db f =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let order_ref fire_id : Access_snapshot.standing_order_ref =
  { order_id = "so_x"; fire_id }

let persist_order ?(room_policy_decision = "") ~db standing_order =
  Access_snapshot.create_and_persist ~db ~config:cfg ~work_type:Standing_order
    ~session_key:"slack:C123" ~room_id:"C123" ~room_policy_decision
    ~standing_order ()

let column_exists db column_name =
  Access_snapshot.sqlite_column_exists ~db ~table_name:"access_snapshots"
    ~column_name

let test_round_trip () =
  with_db (fun db ->
      Access_snapshot.init_schema db;
      Access_snapshot.init_schema db;
      check_bool "order id column added" true
        (column_exists db "standing_order_id");
      check_bool "fire id column added" true
        (column_exists db "standing_order_fire_id");
      let s = persist_order ~db (order_ref (Some "fi_y")) in
      (match Access_snapshot.get_by_id ~db s.id with
      | None -> Alcotest.fail "a standing order snapshot must load"
      | Some found ->
          Alcotest.(check string)
            "work type" "standing_order"
            (Access_snapshot.work_type_to_string found.work_type);
          check_bool "work type parses back" true
            (Access_snapshot.work_type_of_string "standing_order"
            = Some Access_snapshot.Standing_order);
          check_bool "order and fire ref read back" true
            (found.standing_order = Some (order_ref (Some "fi_y")));
          check_bool "row decodes unchanged" true (found = s);
          let json = Access_snapshot.to_json found in
          let ref_json = get "standing_order" json in
          Alcotest.(check string)
            "json order id" "so_x"
            (get "order_id" ref_json |> Yojson.Safe.Util.to_string);
          Alcotest.(check string)
            "json fire id" "fi_y"
            (get "fire_id" ref_json |> Yojson.Safe.Util.to_string));
      let no_fire = persist_order ~db (order_ref None) in
      (match Access_snapshot.get_by_id ~db no_fire.id with
      | None -> Alcotest.fail "a reconcile snapshot must load"
      | Some found ->
          check_bool "missing fire id reads back as None" true
            (found.standing_order = Some (order_ref None));
          check_bool "json fire id is null" true
            (get "fire_id"
               (get "standing_order" (Access_snapshot.to_json found))
            = `Null));
      let plain =
        Access_snapshot.create_and_persist ~db ~config:cfg ~work_type:Room_turn
          ~session_key:"slack:C123" ~room_id:"C123" ()
      in
      match Access_snapshot.get_by_id ~db plain.id with
      | None -> Alcotest.fail "a room turn snapshot must load"
      | Some found ->
          check_bool "no ref without an order id" true
            (found.standing_order = None);
          check_bool "json ref is null" true
            (get "standing_order" (Access_snapshot.to_json found) = `Null))

let test_channel_profile () =
  let config =
    Config_loader.parse_config
      (Yojson.Safe.from_string
         {|{
           "room_profiles": [
             {"id": "so", "model": "openai:gpt-5.4", "denied_tools": ["x"]}
           ],
           "room_profile_bindings": [
             {"profile_id": "so", "room": "C123", "active": true}
           ]
         }|})
  in
  let s =
    Access_snapshot.create ~config ~work_type:Standing_order
      ~session_key:"slack:C123" ~room_id:"C123"
      ~standing_order:(order_ref (Some "fi_y")) ()
  in
  Alcotest.(check (option string)) "channel profile" (Some "so") s.profile_id;
  check_bool "profile denial applies" true (List.mem "x" s.denied_tools)

let test_huddle_resolve_refuses () =
  with_fixture (fun f ->
      f.turn (fun () ->
          let use work_type ?standing_order () =
            let s =
              Access_snapshot.create_and_persist ~db:f.db ~config:!(f.cfg)
                ~work_type ~session_key:f.key ~room_id:"C123"
                ~room_policy_decision:"allow" ?standing_order ()
            in
            f.agent.Agent.access_snapshot_id <- Some s.id;
            f.agent.access_snapshot <- Some s
          in
          use Access_snapshot.Standing_order
            ~standing_order:(order_ref (Some "fi_y")) ();
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          no_content f;
          use Access_snapshot.Room_turn ();
          let* control = f.invoke "slack_huddles" in
          check_bool "same snapshot as a room turn is admitted" false
            (String.starts_with ~prefix:"Error:" control);
          Lwt.return_unit))

let test_github_refuses () =
  with_db (fun db ->
      Access_snapshot.init_schema db;
      let room_for snapshot_id =
        Github_room_tools.room_for_context ~db ~tool_name:"github_room_issue"
          (Some { Tool.default_context with snapshot_id = Some snapshot_id })
      in
      let s =
        persist_order ~db ~room_policy_decision:"allow"
          (order_ref (Some "fi_y"))
      in
      (match room_for s.id with
      | Ok room ->
          Alcotest.fail ("a standing order snapshot authorized room " ^ room)
      | Error message ->
          check_bool message true (contains message "unavailable"));
      let control =
        Access_snapshot.create_and_persist ~db ~config:cfg ~work_type:Room_turn
          ~session_key:"slack:C123" ~room_id:"C123"
          ~room_policy_decision:"allow" ()
      in
      check_bool "same snapshot as a room turn is admitted" true
        (room_for control.id = Ok "C123"))

let suite =
  [
    ("round trip", `Quick, test_round_trip);
    ("channel profile", `Quick, test_channel_profile);
    ("huddle resolve refuses", `Quick, test_huddle_resolve_refuses);
    ("github refuses", `Quick, test_github_refuses);
  ]
