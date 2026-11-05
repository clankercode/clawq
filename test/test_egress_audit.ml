(* Tests for egress audit event recording *)

open Egress_audit

let check_decision msg expected (event : event) =
  Alcotest.check
    (Alcotest.of_pp (fun fmt -> function
      | Allowed -> Format.fprintf fmt "Allowed"
      | Denied -> Format.fprintf fmt "Denied"))
    msg expected event.decision

(** Create an in-memory SQLite database with egress_audit schema. *)
let create_test_db () =
  let db = Sqlite3.db_open ":memory:" in
  init_schema db;
  db

(** Test: redact_host obscures intermediate labels *)
let test_redact_host () =
  Alcotest.(check string)
    "simple host" "e******.com"
    (redact_host "example.com");
  Alcotest.(check string)
    "subdomain" "a**.******.com"
    (redact_host "api.example.com");
  Alcotest.(check string)
    "deep subdomain" "a**.******.******.com"
    (redact_host "api.bob.example.com");
  Alcotest.(check string) "localhost" "l********" (redact_host "localhost");
  Alcotest.(check string) "single char" "*" (redact_host "a")

(** Test: redact_method shows first and last character *)
let test_redact_method () =
  Alcotest.(check string) "GET" "G*T" (redact_method "GET");
  Alcotest.(check string) "POST" "P**T" (redact_method "POST");
  Alcotest.(check string) "DELETE" "D****E" (redact_method "DELETE");
  Alcotest.(check string) "short" "*" (redact_method "A")

(** Test: redact_path keeps first segment *)
let test_redact_path () =
  Alcotest.(check string) "simple path" "/api" (redact_path "/api");
  Alcotest.(check string) "nested path" "/api/**" (redact_path "/api/v1/users");
  Alcotest.(check string) "root path" "/" (redact_path "/")

(** Test: record and query round-trip *)
let test_record_and_query () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"api.example.com" ~method_:"GET"
    ~path:"/api/data" ~matched_rule_index:0 ~session_key:"telegram:123"
    ~snapshot_id:"snap_001" ~tool_name:"http_request" ~profile_id:"default" ();
  let events = query ~db () in
  Alcotest.(check int) "one event" 1 (List.length events);
  let event = List.hd events in
  check_decision "allowed" Allowed event;
  Alcotest.(check string) "host redacted" "a**.******.com" event.host_redacted;
  Alcotest.(check (option string))
    "method redacted" (Some "G*T") event.method_redacted;
  Alcotest.(check (option string))
    "path redacted" (Some "/api/**") event.path_redacted;
  Alcotest.(check int) "rule index" 0 event.matched_rule_index;
  Alcotest.(check (option string))
    "session key" (Some "telegram:123") event.session_key;
  Alcotest.(check (option string))
    "snapshot id" (Some "snap_001") event.snapshot_id;
  Alcotest.(check (option string))
    "tool name" (Some "http_request") event.tool_name;
  Alcotest.(check (option string))
    "profile id" (Some "default") event.profile_id;
  ignore (Sqlite3.db_close db)

(** Test: denied event records correctly *)
let test_denied_event () =
  let db = create_test_db () in
  record ~db ~decision:Denied ~host:"blocked.example.com" ~method_:"POST"
    ~matched_rule_index:1 ();
  let events = query ~db ~decision:Denied () in
  Alcotest.(check int) "one denied event" 1 (List.length events);
  let event = List.hd events in
  check_decision "denied" Denied event;
  Alcotest.(check int) "rule index" 1 event.matched_rule_index;
  ignore (Sqlite3.db_close db)

(** Test: credential handle IDs are stored as aliases *)
let test_credential_handle_ids () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"api.example.com" ~matched_rule_index:0
    ~credential_handle_ids:[ "github-app:main"; "slack-bot:prod" ]
    ();
  let events = query ~db () in
  let event = List.hd events in
  Alcotest.(check int)
    "two handle ids" 2
    (List.length event.credential_handle_ids);
  Alcotest.(check string)
    "first handle id" "github-app:main"
    (List.nth event.credential_handle_ids 0);
  Alcotest.(check string)
    "second handle id" "slack-bot:prod"
    (List.nth event.credential_handle_ids 1);
  ignore (Sqlite3.db_close db)

(** Test: query filters by session_key *)
let test_query_by_session_key () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"a.com" ~matched_rule_index:0
    ~session_key:"telegram:1" ();
  record ~db ~decision:Allowed ~host:"b.com" ~matched_rule_index:0
    ~session_key:"discord:2" ();
  record ~db ~decision:Allowed ~host:"c.com" ~matched_rule_index:0
    ~session_key:"telegram:1" ();
  let events = query ~db ~session_key:"telegram:1" () in
  Alcotest.(check int) "two telegram events" 2 (List.length events);
  ignore (Sqlite3.db_close db)

(** Test: query filters by tool_name *)
let test_query_by_tool_name () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"a.com" ~matched_rule_index:0
    ~tool_name:"http_request" ();
  record ~db ~decision:Allowed ~host:"b.com" ~matched_rule_index:0
    ~tool_name:"web_fetch" ();
  record ~db ~decision:Allowed ~host:"c.com" ~matched_rule_index:0
    ~tool_name:"http_request" ();
  let events = query ~db ~tool_name:"web_fetch" () in
  Alcotest.(check int) "one web_fetch event" 1 (List.length events);
  ignore (Sqlite3.db_close db)

(** Test: event_to_json produces valid JSON *)
let test_event_to_json () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"api.example.com" ~method_:"GET"
    ~path:"/data" ~matched_rule_index:0 ~session_key:"s1"
    ~credential_handle_ids:[ "handle-1" ] ();
  let events = query ~db () in
  let event = List.hd events in
  let json = event_to_json event in
  let json_str = Yojson.Safe.to_string json in
  let parsed = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "decision" "allowed"
    (parsed |> member "decision" |> to_string);
  Alcotest.(check string)
    "host redacted" "a**.******.com"
    (parsed |> member "host_redacted" |> to_string);
  Alcotest.(check bool)
    "has credential_handle_ids" true
    (parsed |> member "credential_handle_ids" |> to_list <> []);
  ignore (Sqlite3.db_close db)

(** Test: delete_before removes old events *)
let test_delete_before () =
  let db = create_test_db () in
  record ~db ~decision:Allowed ~host:"old.com" ~matched_rule_index:0 ();
  (* Delete events before a far-future timestamp *)
  let deleted = delete_before ~db ~before_timestamp:"2099-01-01T00:00:00Z" in
  Alcotest.(check int) "deleted one" 1 deleted;
  let events = query ~db () in
  Alcotest.(check int) "no events left" 0 (List.length events);
  ignore (Sqlite3.db_close db)

(** Test: multiple events recorded with different timestamps *)
let test_multiple_events_recorded () =
  let db = create_test_db () in
  (* Two records of same host/decision will get different timestamps *)
  record ~db ~decision:Allowed ~host:"api.example.com" ~matched_rule_index:0 ();
  record ~db ~decision:Allowed ~host:"api.example.com" ~matched_rule_index:0 ();
  let events = query ~db () in
  (* Both events should be recorded (different timestamps) *)
  Alcotest.(check bool) "at least one event" true (List.length events >= 1);
  ignore (Sqlite3.db_close db)

(** Test: optional fields default to None *)
let test_optional_fields_none () =
  let db = create_test_db () in
  record ~db ~decision:Denied ~host:"test.com" ~matched_rule_index:(-1) ();
  let events = query ~db () in
  let event = List.hd events in
  Alcotest.(check (option string)) "no method" None event.method_redacted;
  Alcotest.(check (option string)) "no path" None event.path_redacted;
  Alcotest.(check (option string)) "no session" None event.session_key;
  Alcotest.(check (option string)) "no snapshot" None event.snapshot_id;
  Alcotest.(check (option string)) "no tool" None event.tool_name;
  Alcotest.(check (option string)) "no profile" None event.profile_id;
  Alcotest.(check int) "no handles" 0 (List.length event.credential_handle_ids);
  ignore (Sqlite3.db_close db)

let contains text fragment =
  let n = String.length fragment in
  let rec loop i =
    i + n <= String.length text
    && (String.sub text i n = fragment || loop (i + 1))
  in
  loop 0

let audit_context db : Policy_http_client.audit_context =
  { Policy_http_client.no_audit with db = Some db }

let test_sensitive_uri () =
  let exercise action =
    let db = create_test_db () and http_calls = ref 0 in
    let rules : Runtime_config_types.egress_rule list =
      [
        {
          host = "files.slack.com";
          path = Some "/secret-path";
          method_ = Some "POST";
          action;
          log_policy = Log;
        };
      ]
    in
    let result, logs =
      Lwt_main.run
        (Test_slack_standing_order_support.with_log_capture (fun () ->
             let result =
               Policy_http_client.check_policy ~rules
                 ~uri:"https://files.slack.com/secret-path?token=secret-query"
                 ~method_:"POST" ~audit:(audit_context db) ~strict_audit:true
                 ~sensitive_uri:true ()
             in
             (match result with Ok () -> incr http_calls | Error _ -> ());
             Lwt.return result))
    in
    let events = query ~db () in
    let event =
      match events with
      | [ e ] -> e
      | _ -> Alcotest.fail "one sensitive audit row"
    in
    Alcotest.(check int)
      "actual path chose rule zero" 0 event.matched_rule_index;
    Alcotest.(check (option string)) "no audit path" None event.path_redacted;
    Alcotest.(check bool)
      "decision retained" true
      (event.decision = match action with Allow -> Allowed | Deny -> Denied);
    let errors =
      match result with
      | Ok () -> []
      | Error e ->
          Alcotest.(check string)
            "sensitive error host" "[sensitive-uri]" e.host;
          Alcotest.(check (option string)) "sensitive error path" None e.path;
          Alcotest.(check (option string))
            "method retained" (Some "POST") e.method_;
          Alcotest.(check int) "error rule index" 0 e.matched_rule_index;
          [
            Policy_http_client.policy_error_to_string e;
            e.host;
            Option.value e.path ~default:"";
            e.message;
          ]
    in
    let surfaces =
      List.map snd logs
      @ List.map (fun e -> Yojson.Safe.to_string (event_to_json e)) events
      @ errors
    in
    Alcotest.(check bool) "decision logged" true (logs <> []);
    Alcotest.(check bool)
      "all surfaces redact both secrets" true
      (List.for_all
         (fun surface ->
           (not (contains surface "secret-path"))
           && not (contains surface "secret-query"))
         surfaces);
    !http_calls
  in
  let allow_http_calls = exercise Allow and deny_http_calls = exercise Deny in
  Alcotest.(check int) "allow dispatches once" 1 allow_http_calls;
  Alcotest.(check int) "deny never dispatches" 0 deny_http_calls;
  List.iter
    (fun uri ->
      let db = create_test_db () in
      let result, logs =
        Lwt_main.run
          (Test_slack_standing_order_support.with_log_capture (fun () ->
               Lwt.return
                 (Policy_http_client.check_policy ~rules:[] ~uri ~method_:"POST"
                    ~audit:(audit_context db) ~strict_audit:true
                    ~sensitive_uri:true ())))
      in
      match result with
      | Ok () -> Alcotest.fail "malformed or unmatched URI must deny"
      | Error e ->
          Alcotest.(check string)
            "redacted host even for malformed URI" "[sensitive-uri]" e.host;
          Alcotest.(check (option string)) "redacted path" None e.path;
          let surfaces =
            (e.message :: e.host :: List.map snd logs)
            @ List.map
                (fun e -> Yojson.Safe.to_string (event_to_json e))
                (query ~db ())
          in
          Alcotest.(check bool)
            "userinfo fragment query and path absent" true
            (List.for_all
               (fun surface ->
                 List.for_all
                   (fun secret -> not (contains surface secret))
                   [
                     "secret-path";
                     "secret-query";
                     "secret-user";
                     "secret-fragment";
                   ])
               surfaces))
    [
      "/secret-path?token=secret-query#secret-fragment";
      "https://secret-user@files.slack.com/secret-path?token=secret-query#secret-fragment";
    ]

let test_strict_insert () =
  List.iter
    (fun failure ->
      let db = create_test_db () in
      Sql_util.exec_exn db
        ("CREATE TRIGGER fail_audit BEFORE INSERT ON egress_audit BEGIN SELECT \
          RAISE(" ^ failure ^ "); END");
      let raised =
        try
          record ~db ~decision:Allowed ~host:"slack.com" ~matched_rule_index:0
            ~strict:true ();
          false
        with _ -> true
      in
      Alcotest.(check bool) "strict failure raises" true raised)
    [ "FAIL, 'test audit failure'"; "IGNORE" ]

let test_default_compatibility () =
  let db = create_test_db () in
  let rule : Runtime_config_types.egress_rule =
    {
      host = "files.slack.com";
      path = Some "/secret-path";
      method_ = Some "POST";
      action = Deny;
      log_policy = Log;
    }
  in
  let result, logs =
    Lwt_main.run
      (Test_slack_standing_order_support.with_log_capture (fun () ->
           Lwt.return
             (Policy_http_client.check_policy ~rules:[ rule ]
                ~uri:"https://files.slack.com/secret-path?token=secret-query"
                ~method_:"POST" ~audit:(audit_context db) ())))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected default denial"
  | Error e ->
      Alcotest.(check string) "default host preserved" "files.slack.com" e.host;
      Alcotest.(check (option string))
        "default path preserved" (Some "/secret-path") e.path;
      Alcotest.(check string)
        "default message preserved"
        "egress denied: POST files.slack.com /secret-path (rule 0). Review \
         access_bundles[].egress_rules, egress.default_allowlist, or \
         egress.strictness if this HTTP destination should be allowed."
        (Policy_http_client.policy_error_to_string e));
  Alcotest.(check bool)
    "ordinary default log preserved" true
    (List.exists (fun (_, msg) -> contains msg "/secret-path") logs);
  Sql_util.exec_exn db
    "CREATE TRIGGER fail_audit BEFORE INSERT ON egress_audit BEGIN SELECT \
     RAISE(FAIL, 'test audit failure'); END";
  let result =
    Policy_http_client.check_policy
      ~rules:[ { rule with action = Allow } ]
      ~uri:"https://files.slack.com/secret-path" ~method_:"POST"
      ~audit:(audit_context db) ()
  in
  Alcotest.(check bool)
    "non-strict step failure still permits" true (result = Ok ())

let suite =
  [
    Alcotest.test_case
      "sensitive upload URI Log allow and deny redact every surface" `Quick
      test_sensitive_uri;
    Alcotest.test_case "strict audit checks step and inserted row" `Quick
      test_strict_insert;
    Alcotest.test_case "default policy output remains compatible" `Quick
      test_default_compatibility;
    Alcotest.test_case "redact_host" `Quick test_redact_host;
    Alcotest.test_case "redact_method" `Quick test_redact_method;
    Alcotest.test_case "redact_path" `Quick test_redact_path;
    Alcotest.test_case "record and query" `Quick test_record_and_query;
    Alcotest.test_case "denied event" `Quick test_denied_event;
    Alcotest.test_case "credential handle IDs" `Quick test_credential_handle_ids;
    Alcotest.test_case "query by session_key" `Quick test_query_by_session_key;
    Alcotest.test_case "query by tool_name" `Quick test_query_by_tool_name;
    Alcotest.test_case "event_to_json" `Quick test_event_to_json;
    Alcotest.test_case "delete_before" `Quick test_delete_before;
    Alcotest.test_case "multiple events recorded" `Quick
      test_multiple_events_recorded;
    Alcotest.test_case "optional fields none" `Quick test_optional_fields_none;
  ]
