(* Tests for egress decision flow and audit redaction.

   Covers:
   - Host/path/method matching through the full decision path
   - Deny precedence (deny rules placed before allow rules)
   - Read-only denial (allow GET, deny write methods)
   - Audit mode (check_policy with audit context records to SQLite)
   - Secret redaction (audit records never contain raw host/method/path) *)

open Runtime_config

(* -- Helpers -- *)

let mk_rule ?path ?method_ ?(log_policy = Log) host action : egress_rule =
  { host; path; method_; action; log_policy }

let allow ?path ?method_ ?log_policy host =
  mk_rule ?path ?method_ ?log_policy host Allow

let deny ?path ?method_ ?log_policy host =
  mk_rule ?path ?method_ ?log_policy host Deny

let create_audit_db () =
  let db = Sqlite3.db_open ":memory:" in
  Egress_audit.init_schema db;
  db

let audit_ctx ?(session_key = "test-session") ?(tool_name = "test-tool") db :
    Policy_http_client.audit_context =
  {
    db = Some db;
    session_key = Some session_key;
    snapshot_id = Some "snap-001";
    tool_name = Some tool_name;
    profile_id = Some "default";
    credential_handle_ids = [ "cred-handle-1" ];
  }

let check_allow msg rules uri ?method_ () =
  let result = Policy_http_client.check_policy ~rules ~uri ?method_ () in
  match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: expected allow, got deny: %s" msg e.message

let check_deny msg rules uri ?method_ () =
  let result = Policy_http_client.check_policy ~rules ~uri ?method_ () in
  match result with
  | Error e -> e
  | Ok () -> Alcotest.failf "%s: expected deny, but was allowed" msg

let query_audit_events db = Egress_audit.query ~db ()

(* ================================================================
   Section 1: Host matching through full decision path
   ================================================================ *)

let test_exact_host_match () =
  let rules = [ allow "api.example.com"; deny "*" ] in
  check_allow "exact host allows" rules "https://api.example.com/data" ();
  let err = check_deny "other host denies" rules "https://other.com/data" () in
  Alcotest.(check string) "denied host" "other.com" err.host

let test_wildcard_host_match () =
  let rules = [ allow "*.example.com"; deny "*" ] in
  check_allow "subdomain matches" rules "https://api.example.com/x" ();
  check_allow "deep subdomain matches" rules "https://deep.api.example.com/x" ();
  let err = check_deny "bare domain denies" rules "https://example.com/x" () in
  Alcotest.(check string) "denied host" "example.com" err.host

let test_catchall_deny () =
  let rules = [ deny "*" ] in
  let err =
    check_deny "catch-all denies everything" rules "https://anything.org/x" ()
  in
  Alcotest.(check string) "host" "anything.org" err.host

let test_host_case_insensitive () =
  let rules = [ allow "api.example.com" ] in
  (* URI host is lowercased by parse_uri *)
  check_allow "case insensitive host" rules "https://API.EXAMPLE.COM/data" ()

(* ================================================================
   Section 2: Path matching through full decision path
   ================================================================ *)

let test_path_wildcard_match () =
  let rules =
    [ allow ~path:"/api/v1/*" "api.example.com"; deny "api.example.com" ]
  in
  check_allow "wildcard path allows" rules
    "https://api.example.com/api/v1/users" ();
  check_allow "nested path allows" rules
    "https://api.example.com/api/v1/users/123" ();
  let err =
    check_deny "non-matching path denies" rules
      "https://api.example.com/api/v2/users" ()
  in
  Alcotest.(check (option string)) "denied path" (Some "/api/v2/users") err.path

let test_exact_path_match () =
  let rules =
    [ allow ~path:"/health" "api.example.com"; deny "api.example.com" ]
  in
  check_allow "exact path allows" rules "https://api.example.com/health" ();
  let err =
    check_deny "wrong path denies" rules "https://api.example.com/status" ()
  in
  Alcotest.(check (option string)) "denied path" (Some "/status") err.path

let test_path_required_but_mismatched () =
  let rules = [ allow ~path:"/v1/*" "api.example.com" ] in
  let err =
    check_deny "path mismatch denies" rules "https://api.example.com/" ()
  in
  Alcotest.(check string) "host" "api.example.com" err.host

(* ================================================================
   Section 3: Method matching through full decision path
   ================================================================ *)

let test_method_restriction () =
  let rules =
    [
      allow ~method_:"GET" "api.example.com";
      deny ~method_:"DELETE" "api.example.com";
    ]
  in
  check_allow "GET allowed" rules "https://api.example.com/data" ~method_:"GET"
    ();
  let err =
    check_deny "DELETE denied" rules "https://api.example.com/data/1"
      ~method_:"DELETE" ()
  in
  Alcotest.(check (option string)) "denied method" (Some "DELETE") err.method_

let test_method_case_insensitive () =
  let rules = [ allow ~method_:"GET" "api.example.com" ] in
  check_allow "lowercase get" rules "https://api.example.com/data"
    ~method_:"get" ();
  check_allow "mixed Get" rules "https://api.example.com/data" ~method_:"Get" ()

let test_method_required_but_missing () =
  let rules = [ allow ~method_:"POST" "api.example.com" ] in
  let err =
    check_deny "no method denies when required" rules
      "https://api.example.com/data" ()
  in
  Alcotest.(check (option string)) "no method" None err.method_

(* ================================================================
   Section 4: Deny precedence
   ================================================================ *)

let test_deny_before_allow_takes_effect () =
  (* When deny rule is placed before allow, deny wins *)
  let rules =
    [
      deny ~path:"/admin/*" "api.example.com"; allow "api.example.com"; deny "*";
    ]
  in
  check_allow "non-admin allowed" rules "https://api.example.com/public" ();
  let err =
    check_deny "admin path denied" rules "https://api.example.com/admin/users"
      ()
  in
  Alcotest.(check int) "deny rule index" 0 err.matched_rule_index;
  check_allow "public path allowed" rules "https://api.example.com/public/data"
    ()

let test_deny_before_allow_same_host () =
  (* Deny specific write paths, allow everything else *)
  let rules =
    [
      deny ~path:"/internal/*" "service.corp";
      deny ~path:"/admin/*" "service.corp";
      allow "service.corp";
    ]
  in
  check_allow "normal path allowed" rules "https://service.corp/api/data" ();
  let err =
    check_deny "internal denied" rules "https://service.corp/internal/secrets"
      ()
  in
  Alcotest.(check int) "matched deny rule" 0 err.matched_rule_index;
  let err2 =
    check_deny "admin denied" rules "https://service.corp/admin/config" ()
  in
  Alcotest.(check int) "matched deny rule" 1 err2.matched_rule_index

let test_deny_method_before_allow () =
  (* Deny DELETE, allow everything else *)
  let rules =
    [
      deny ~method_:"DELETE" "api.example.com";
      allow "api.example.com";
      deny "*";
    ]
  in
  check_allow "GET allowed" rules "https://api.example.com/data" ~method_:"GET"
    ();
  check_allow "POST allowed" rules "https://api.example.com/data"
    ~method_:"POST" ();
  let err =
    check_deny "DELETE denied" rules "https://api.example.com/data/1"
      ~method_:"DELETE" ()
  in
  Alcotest.(check int) "deny rule index" 0 err.matched_rule_index

let test_catchall_deny_after_specific_allows () =
  let rules = [ allow "api.github.com"; allow "*.trusted.com"; deny "*" ] in
  check_allow "github allowed" rules "https://api.github.com/repos" ();
  check_allow "trusted allowed" rules "https://sub.trusted.com/data" ();
  let err =
    check_deny "everything else denied" rules "https://malicious.com/x" ()
  in
  Alcotest.(check int) "catch-all index" 2 err.matched_rule_index

(* ================================================================
   Section 5: Read-only denial pattern
   ================================================================ *)

let test_read_only_egress () =
  (* Allow GET only, deny all write methods *)
  let rules =
    [
      allow ~method_:"GET" "api.example.com";
      allow ~method_:"HEAD" "api.example.com";
      deny "api.example.com";
    ]
  in
  check_allow "GET allowed" rules "https://api.example.com/data" ~method_:"GET"
    ();
  check_allow "HEAD allowed" rules "https://api.example.com/data"
    ~method_:"HEAD" ();
  let err_post =
    check_deny "POST denied" rules "https://api.example.com/data"
      ~method_:"POST" ()
  in
  Alcotest.(check int) "POST matched deny" 2 err_post.matched_rule_index;
  let err_put =
    check_deny "PUT denied" rules "https://api.example.com/data" ~method_:"PUT"
      ()
  in
  Alcotest.(check int) "PUT matched deny" 2 err_put.matched_rule_index;
  let err_del =
    check_deny "DELETE denied" rules "https://api.example.com/data/1"
      ~method_:"DELETE" ()
  in
  Alcotest.(check int) "DELETE matched deny" 2 err_del.matched_rule_index;
  let err_patch =
    check_deny "PATCH denied" rules "https://api.example.com/data/1"
      ~method_:"PATCH" ()
  in
  Alcotest.(check int) "PATCH matched deny" 2 err_patch.matched_rule_index

let test_read_only_with_path () =
  (* Read-only for /api/*, deny everything else on the host *)
  let rules =
    [ allow ~path:"/api/*" ~method_:"GET" "data.corp"; deny "data.corp" ]
  in
  check_allow "GET /api/data allowed" rules "https://data.corp/api/data"
    ~method_:"GET" ();
  let err =
    check_deny "POST /api/data denied" rules "https://data.corp/api/data"
      ~method_:"POST" ()
  in
  Alcotest.(check int) "matched deny" 1 err.matched_rule_index;
  let err2 =
    check_deny "GET /other denied" rules "https://data.corp/other"
      ~method_:"GET" ()
  in
  Alcotest.(check int) "path mismatch deny" 1 err2.matched_rule_index

(* ================================================================
   Section 6: Audit mode -- check_policy with audit context
   ================================================================ *)

let test_audit_records_allow () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ allow "api.example.com" ] in
  let result =
    Policy_http_client.check_policy ~rules ~uri:"https://api.example.com/data"
      ~method_:"GET" ~audit ()
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "expected allow: %s" e.message);
  let events = query_audit_events db in
  Alcotest.(check int) "one audit event" 1 (List.length events);
  let evt = List.hd events in
  (match evt.Egress_audit.decision with
  | Egress_audit.Allowed -> ()
  | Egress_audit.Denied -> Alcotest.fail "audit should record Allowed");
  Alcotest.(check int) "rule index" 0 evt.matched_rule_index;
  Alcotest.(check (option string))
    "session key" (Some "test-session") evt.session_key;
  Alcotest.(check (option string)) "tool name" (Some "test-tool") evt.tool_name;
  ignore (Sqlite3.db_close db)

let test_audit_records_deny () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ deny "*" ] in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"https://blocked.example.com/secret" ~method_:"POST" ~audit ()
  in
  (match result with Error _ -> () | Ok () -> Alcotest.fail "expected deny");
  let events = query_audit_events db in
  Alcotest.(check int) "one audit event" 1 (List.length events);
  let evt = List.hd events in
  (match evt.Egress_audit.decision with
  | Egress_audit.Denied -> ()
  | Egress_audit.Allowed -> Alcotest.fail "audit should record Denied");
  Alcotest.(check int) "rule index" 0 evt.matched_rule_index;
  ignore (Sqlite3.db_close db)

let test_audit_records_default_deny () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  (* Empty rules => default deny, matched_rule_index = -1 *)
  let result =
    Policy_http_client.check_policy ~rules:[] ~uri:"https://anything.com/x"
      ~audit ()
  in
  (match result with Error _ -> () | Ok () -> Alcotest.fail "expected deny");
  let events = query_audit_events db in
  let evt = List.hd events in
  Alcotest.(check int) "default deny index" (-1) evt.matched_rule_index;
  ignore (Sqlite3.db_close db)

let test_audit_records_no_events_when_no_audit_ctx () =
  let db = create_audit_db () in
  let rules = [ allow "api.example.com" ] in
  (* No ~audit parameter => no audit recording *)
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://api.example.com/data"
      ()
  in
  let events = query_audit_events db in
  Alcotest.(check int) "no audit events" 0 (List.length events);
  ignore (Sqlite3.db_close db)

let test_audit_multiple_decisions () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ allow "allowed.com"; deny "*" ] in
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://allowed.com/a" ~audit
      ()
  in
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://blocked.com/b" ~audit
      ()
  in
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://also-blocked.com/c"
      ~audit ()
  in
  let events = query_audit_events db in
  Alcotest.(check int) "three audit events" 3 (List.length events);
  let decisions =
    List.map (fun (e : Egress_audit.event) -> e.decision) events
  in
  Alcotest.(check int)
    "one allowed" 1
    (List.length (List.filter (fun d -> d = Egress_audit.Allowed) decisions));
  Alcotest.(check int)
    "two denied" 2
    (List.length (List.filter (fun d -> d = Egress_audit.Denied) decisions));
  ignore (Sqlite3.db_close db)

(* ================================================================
   Section 7: Secret redaction in audit records
   ================================================================ *)

let test_audit_host_redacted () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ allow "api.secret-service.internal.example.com" ] in
  let _ =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.secret-service.internal.example.com/data" ~audit ()
  in
  let events = query_audit_events db in
  let evt = List.hd events in
  (* Host should be redacted -- not the raw hostname *)
  Alcotest.(check bool)
    "host redacted (not raw)" true
    (evt.Egress_audit.host_redacted <> "api.secret-service.internal.example.com");
  (* Redacted host should still contain TLD *)
  Alcotest.(check bool)
    "host contains TLD" true
    (Test_helpers.string_contains evt.Egress_audit.host_redacted "com");
  (* Redacted host should have asterisks *)
  Alcotest.(check bool)
    "host has redaction markers" true
    (Test_helpers.string_contains evt.Egress_audit.host_redacted "*");
  ignore (Sqlite3.db_close db)

let test_audit_method_redacted () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ deny "evil.com" ] in
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://evil.com/steal"
      ~method_:"POST" ~audit ()
  in
  let events = query_audit_events db in
  let evt = List.hd events in
  (match evt.Egress_audit.method_redacted with
  | None -> Alcotest.fail "method should be present"
  | Some redacted ->
      Alcotest.(check bool) "method redacted (not raw)" true (redacted <> "POST");
      Alcotest.(check bool)
        "method has asterisks" true
        (Test_helpers.string_contains redacted "*");
      (* Should show first and last chars: P**T *)
      Alcotest.(check bool)
        "method shows first char" true
        (String.length redacted > 0 && redacted.[0] = 'P');
      Alcotest.(check bool)
        "method shows last char" true
        (String.length redacted > 1
        && redacted.[String.length redacted - 1] = 'T'));
  ignore (Sqlite3.db_close db)

let test_audit_path_redacted () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ allow "api.example.com" ] in
  let _ =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.example.com/api/v1/users/123/secret-data" ~audit ()
  in
  let events = query_audit_events db in
  let evt = List.hd events in
  (match evt.Egress_audit.path_redacted with
  | None -> Alcotest.fail "path should be present"
  | Some redacted ->
      (* Path redaction keeps first segment, obscures the rest *)
      Alcotest.(check bool)
        "path redacted (not raw)" true
        (redacted <> "/api/v1/users/123/secret-data");
      Alcotest.(check bool)
        "path starts with first segment" true
        (Test_helpers.string_contains redacted "/api"));
  ignore (Sqlite3.db_close db)

let test_audit_no_secrets_in_json () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules = [ deny "*" ] in
  let _ =
    Policy_http_client.check_policy ~rules
      ~uri:"https://super-secret-service.internal.corp/api/tokens"
      ~method_:"POST" ~audit ()
  in
  let events = query_audit_events db in
  let evt = List.hd events in
  let json = Egress_audit.event_to_json evt in
  let json_str = Yojson.Safe.to_string json in
  (* Raw hostname must not appear in JSON *)
  Alcotest.(check bool)
    "raw host not in JSON" false
    (Test_helpers.string_contains json_str "super-secret-service.internal.corp");
  (* Raw method must not appear in JSON *)
  Alcotest.(check bool)
    "raw method not in JSON" false
    (Test_helpers.string_contains json_str "\"POST\"");
  (* Raw path must not appear in JSON *)
  Alcotest.(check bool)
    "raw path not in JSON" false
    (Test_helpers.string_contains json_str "/api/tokens");
  ignore (Sqlite3.db_close db)

let test_audit_credential_ids_preserved () =
  (* Credential handle IDs are aliases, not secrets -- they should be stored
     as-is (not redacted). *)
  let db = create_audit_db () in
  let audit =
    {
      (audit_ctx db) with
      credential_handle_ids = [ "github-app:main"; "slack-bot:prod" ];
    }
  in
  let rules = [ allow "api.github.com" ] in
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://api.github.com/repos"
      ~audit ()
  in
  let events = query_audit_events db in
  let evt = List.hd events in
  Alcotest.(check int)
    "two credential ids" 2
    (List.length evt.Egress_audit.credential_handle_ids);
  Alcotest.(check string)
    "first id preserved" "github-app:main"
    (List.nth evt.Egress_audit.credential_handle_ids 0);
  Alcotest.(check string)
    "second id preserved" "slack-bot:prod"
    (List.nth evt.Egress_audit.credential_handle_ids 1);
  ignore (Sqlite3.db_close db)

(* ================================================================
   Section 8: Combined decision + audit scenarios
   ================================================================ *)

let test_read_only_egress_with_audit () =
  let db = create_audit_db () in
  let audit = audit_ctx ~tool_name:"read-only-client" db in
  let rules =
    [
      allow ~method_:"GET" "api.example.com";
      allow ~method_:"HEAD" "api.example.com";
      deny "api.example.com";
    ]
  in
  (* GET allowed + audited *)
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://api.example.com/data"
      ~method_:"GET" ~audit ()
  in
  (* POST denied + audited *)
  let _ =
    Policy_http_client.check_policy ~rules ~uri:"https://api.example.com/data"
      ~method_:"POST" ~audit ()
  in
  let events = query_audit_events db in
  Alcotest.(check int) "two events" 2 (List.length events);
  (* Query returns DESC by timestamp; find by decision *)
  let allowed_evts =
    List.filter
      (fun (e : Egress_audit.event) -> e.decision = Egress_audit.Allowed)
      events
  in
  let denied_evts =
    List.filter
      (fun (e : Egress_audit.event) -> e.decision = Egress_audit.Denied)
      events
  in
  Alcotest.(check int) "one allowed" 1 (List.length allowed_evts);
  Alcotest.(check int) "one denied" 1 (List.length denied_evts);
  (* Both events redacted *)
  List.iter
    (fun (evt : Egress_audit.event) ->
      Alcotest.(check bool)
        "host redacted" true
        (evt.host_redacted <> "api.example.com"))
    events;
  ignore (Sqlite3.db_close db)

let test_deny_precedence_with_audit () =
  let db = create_audit_db () in
  let audit = audit_ctx db in
  let rules =
    [
      deny ~path:"/admin/*" "api.example.com"; allow "api.example.com"; deny "*";
    ]
  in
  (* Admin path denied *)
  let _ =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.example.com/admin/config" ~audit ()
  in
  (* Public path allowed *)
  let _ =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.example.com/public/data" ~audit ()
  in
  let events = query_audit_events db in
  Alcotest.(check int) "two events" 2 (List.length events);
  (* Query returns DESC by timestamp; find by decision *)
  let denied_evts =
    List.filter
      (fun (e : Egress_audit.event) -> e.decision = Egress_audit.Denied)
      events
  in
  let allowed_evts =
    List.filter
      (fun (e : Egress_audit.event) -> e.decision = Egress_audit.Allowed)
      events
  in
  Alcotest.(check int) "one denied" 1 (List.length denied_evts);
  Alcotest.(check int) "one allowed" 1 (List.length allowed_evts);
  let denied_evt = List.hd denied_evts in
  let allowed_evt = List.hd allowed_evts in
  Alcotest.(check int) "admin deny index" 0 denied_evt.matched_rule_index;
  Alcotest.(check int) "public allow index" 1 allowed_evt.matched_rule_index;
  ignore (Sqlite3.db_close db)

(* -- Suite -- *)

let suite =
  [
    (* Host matching *)
    Alcotest.test_case "exact host match" `Quick test_exact_host_match;
    Alcotest.test_case "wildcard host match" `Quick test_wildcard_host_match;
    Alcotest.test_case "catch-all deny" `Quick test_catchall_deny;
    Alcotest.test_case "host case insensitive" `Quick test_host_case_insensitive;
    (* Path matching *)
    Alcotest.test_case "path wildcard match" `Quick test_path_wildcard_match;
    Alcotest.test_case "exact path match" `Quick test_exact_path_match;
    Alcotest.test_case "path required but mismatched" `Quick
      test_path_required_but_mismatched;
    (* Method matching *)
    Alcotest.test_case "method restriction" `Quick test_method_restriction;
    Alcotest.test_case "method case insensitive" `Quick
      test_method_case_insensitive;
    Alcotest.test_case "method required but missing" `Quick
      test_method_required_but_missing;
    (* Deny precedence *)
    Alcotest.test_case "deny before allow takes effect" `Quick
      test_deny_before_allow_takes_effect;
    Alcotest.test_case "deny before allow same host" `Quick
      test_deny_before_allow_same_host;
    Alcotest.test_case "deny method before allow" `Quick
      test_deny_method_before_allow;
    Alcotest.test_case "catch-all deny after allows" `Quick
      test_catchall_deny_after_specific_allows;
    (* Read-only denial *)
    Alcotest.test_case "read-only egress" `Quick test_read_only_egress;
    Alcotest.test_case "read-only with path" `Quick test_read_only_with_path;
    (* Audit mode *)
    Alcotest.test_case "audit records allow" `Quick test_audit_records_allow;
    Alcotest.test_case "audit records deny" `Quick test_audit_records_deny;
    Alcotest.test_case "audit records default deny" `Quick
      test_audit_records_default_deny;
    Alcotest.test_case "audit no events without context" `Quick
      test_audit_records_no_events_when_no_audit_ctx;
    Alcotest.test_case "audit multiple decisions" `Quick
      test_audit_multiple_decisions;
    (* Secret redaction *)
    Alcotest.test_case "audit host redacted" `Quick test_audit_host_redacted;
    Alcotest.test_case "audit method redacted" `Quick test_audit_method_redacted;
    Alcotest.test_case "audit path redacted" `Quick test_audit_path_redacted;
    Alcotest.test_case "audit no secrets in JSON" `Quick
      test_audit_no_secrets_in_json;
    Alcotest.test_case "audit credential IDs preserved" `Quick
      test_audit_credential_ids_preserved;
    (* Combined scenarios *)
    Alcotest.test_case "read-only egress with audit" `Quick
      test_read_only_egress_with_audit;
    Alcotest.test_case "deny precedence with audit" `Quick
      test_deny_precedence_with_audit;
  ]
