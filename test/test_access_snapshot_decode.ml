open Lwt.Syntax
open Test_slack_huddle_fixtures

let cfg =
  Config_loader.parse_config
    (Yojson.Safe.from_string {|{ "workspace": "/tmp/test" }|})

let with_db f =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Access_snapshot.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let persisted db =
  Access_snapshot.create_and_persist ~db ~config:cfg ~work_type:Room_turn
    ~session_key:"slack:C1" ~room_id:"C1" ~room_policy_decision:"allow" ()

let set db sql id = Sql_util.exec_with_params db sql [ Sqlite3.Data.TEXT id ]

let to_unknown db id =
  set db "UPDATE access_snapshots SET work_type='future_work_type' WHERE id=?"
    id

let snapshot_ids snaps = List.map (fun (s : Access_snapshot.t) -> s.id) snaps

let test_unknown_fails_closed () =
  with_db (fun db ->
      let known = persisted db in
      let unknown = persisted db in
      to_unknown db unknown.id;
      check_bool "unknown row is never authority" true
        (Access_snapshot.get_by_id ~db unknown.id = None);
      check_bool "known row still loads" true
        (Access_snapshot.get_by_id ~db known.id <> None);
      Alcotest.(check (list string))
        "query returns known rows only" [ known.id ]
        (snapshot_ids (Access_snapshot.query ~db ()));
      let listed = Access_snapshot.list_rows ~db () in
      Alcotest.(check int) "listing keeps every row" 2 (List.length listed);
      check_bool "listing shows the stored type" true
        (List.exists
           (function
             | Access_snapshot.Unknown
                 { id; stored_work_type; session_key; room_id; _ } ->
                 id = unknown.id
                 && stored_work_type = "future_work_type"
                 && session_key = Some "slack:C1"
                 && room_id = Some "C1"
             | Access_snapshot.Known _ -> false)
           listed);
      Test_helpers.with_temp_home (fun dir ->
          let path = Filename.concat dir "snapshots.jsonl" in
          Alcotest.(check int)
            "export counts every row" 2
            (Access_snapshot.export_json ~db ~path ());
          let text = In_channel.with_open_bin path In_channel.input_all in
          check_bool "export marks unknown" true
            (contains text {|"work_type":"unknown"|});
          check_bool "export keeps the stored type" true
            (contains text {|"stored_work_type":"future_work_type"|})))

let test_corrupt_classification () =
  with_db (fun db ->
      let s = persisted db in
      check_bool "created unclassified" true
        (s.room_classification = Runtime_config_types.Rm_unknown);
      set db
        "UPDATE access_snapshots SET room_classification='not json' WHERE id=?"
        s.id;
      match Access_snapshot.get_by_id ~db s.id with
      | None -> Alcotest.fail "a corrupt classification must not hide the row"
      | Some found ->
          check_bool "classification decodes Rm_unknown" true
            (found.room_classification = Runtime_config_types.Rm_unknown);
          check_bool "row decodes unchanged" true (found = s))

let test_huddle_tools_deny () =
  with_fixture (fun f ->
      f.turn (fun () ->
          check_bool "snapshot rewritten" true
            (Sqlite3.exec f.db
               "UPDATE access_snapshots SET work_type='future_work_type'"
            = Sqlite3.Rc.OK);
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool "denied before the network" true (!(f.calls) = []);
          Lwt.return_unit))

let test_github_room_tools_deny () =
  with_db (fun db ->
      let s = persisted db in
      to_unknown db s.id;
      let tool =
        match
          List.find_opt
            (fun (t : Tool.t) -> t.name = "github_room_get_item")
            (Github_room_tools.runtime_tools ~db ~config:cfg)
        with
        | Some tool -> tool
        | None -> Alcotest.fail "github_room_get_item is not a runtime tool"
      in
      let context = { Tool.default_context with snapshot_id = Some s.id } in
      let output =
        Lwt_main.run
          (tool.invoke ~context
             (`Assoc [ ("item_key", `String "pr:acme/widget:42") ]))
      in
      expect_error output;
      check_bool output true (contains output "unavailable"))

let suite =
  [
    ( "unknown work type fails closed; listing shows unknown",
      `Quick,
      test_unknown_fails_closed );
    ( "corrupt classification decodes Rm_unknown",
      `Quick,
      test_corrupt_classification );
    ("huddle tools deny unknown work type", `Quick, test_huddle_tools_deny);
    ( "github room tools deny unknown work type",
      `Quick,
      test_github_room_tools_deny );
  ]
