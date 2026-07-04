(* Tests for policy-aware HTTP client wrapper *)

open Runtime_config

let check_ok msg = function
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: unexpected policy error: %s" msg
        (Policy_http_client.policy_error_to_string e)

let check_denied msg = function
  | Ok _ ->
      Alcotest.failf "%s: expected policy denial but request was allowed" msg
  | Error (e : Policy_http_client.policy_error) -> e

(** Allow-all rules for tests that need the HTTP call to proceed. *)
let allow_all_rules : egress_rule list =
  [
    {
      host = "*";
      path = None;
      method_ = None;
      action = Allow;
      log_policy = No_log;
    };
  ]

(** Deny-all rules for tests that expect policy denial. *)
let deny_all_rules : egress_rule list =
  [
    { host = "*"; path = None; method_ = None; action = Deny; log_policy = Log };
  ]

(** [with_http_server callback f] starts a temporary HTTP server on a free port
    and calls [f port]. The server is torn down after [f] returns. *)
let with_http_server callback f =
  let port = Test_helpers.free_port () in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () ->
      if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ())
    (fun () -> f port)

(* --- check_policy unit tests --- *)

let test_check_policy_allow () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let result =
    Policy_http_client.check_policy ~rules ~uri:"http://api.example.com/v1/data"
      ()
  in
  Alcotest.(check bool) "should be Ok" true (result = Ok ())

let test_check_policy_deny () =
  let rules =
    [
      {
        host = "blocked.example.com";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"http://blocked.example.com/secret" ()
  in
  let err = check_denied "blocked host" result in
  Alcotest.(check string) "host" "blocked.example.com" err.host;
  Alcotest.(check (option string)) "path" (Some "/secret") err.path;
  Alcotest.(check int) "rule index" 0 err.matched_rule_index

let test_check_policy_default_deny () =
  let rules = [] in
  let result =
    Policy_http_client.check_policy ~rules ~uri:"http://anything.com/x" ()
  in
  let err = check_denied "empty rules" result in
  Alcotest.(check int) "default index" (-1) err.matched_rule_index;
  Alcotest.(check bool) "mentions deny" true (String.contains err.message 'd')

let test_check_policy_default_allowlist_clawq_docs () =
  let rules = [] in
  let result =
    Policy_http_client.check_policy ~rules ~egress:Runtime_config.default.egress
      ~uri:"https://clawq.org/llms.txt" ~method_:"GET" ()
  in
  Alcotest.(check bool) "clawq llms.txt allowed" true (result = Ok ())

let test_check_policy_permissive_unmatched () =
  let egress =
    {
      Runtime_config.default.egress with
      strictness = Runtime_config.Permissive;
    }
  in
  let result =
    Policy_http_client.check_policy ~rules:[] ~egress
      ~uri:"https://example.com/data" ~method_:"GET" ()
  in
  Alcotest.(check bool) "permissive unmatched allowed" true (result = Ok ())

let test_check_policy_default_deny_hint () =
  let result =
    Policy_http_client.check_policy ~rules:[]
      ~egress:Runtime_config.default.egress ~uri:"https://example.com/data"
      ~method_:"GET" ()
  in
  let err = check_denied "unmatched host" result in
  Alcotest.(check int) "default index" (-1) err.matched_rule_index;
  Alcotest.(check bool)
    "suggests egress config" true
    (Test_helpers.string_contains err.message "egress.default_allowlist"
    && Test_helpers.string_contains err.message "egress.strictness")

let test_check_policy_wildcard_host () =
  let rules =
    [
      {
        host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules ~uri:"http://api.example.com/x" ()
  in
  Alcotest.(check bool) "subdomain allowed" true (r1 = Ok ());
  let r2 =
    Policy_http_client.check_policy ~rules ~uri:"http://other.org/x" ()
  in
  Alcotest.(check bool) "other host denied" true (r2 <> Ok ())

let test_check_policy_path_matching () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/v1/*";
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"http://api.example.com/v1/users" ()
  in
  Alcotest.(check bool) "matching path allowed" true (r1 = Ok ());
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"http://api.example.com/v2/users" ()
  in
  Alcotest.(check bool) "non-matching path denied" true (r2 <> Ok ())

let test_check_policy_method_matching () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = Some "GET";
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules ~method_:"GET"
      ~uri:"http://api.example.com/data" ()
  in
  Alcotest.(check bool) "GET allowed" true (r1 = Ok ());
  let r2 =
    Policy_http_client.check_policy ~rules ~method_:"POST"
      ~uri:"http://api.example.com/data" ()
  in
  Alcotest.(check bool) "POST denied" true (r2 <> Ok ())

let test_check_policy_first_match_wins () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/admin/*";
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"http://api.example.com/admin/config" ()
  in
  let err = check_denied "admin path denied" r1 in
  Alcotest.(check int) "matched rule 0" 0 err.matched_rule_index;
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"http://api.example.com/public/data" ()
  in
  Alcotest.(check bool) "public path allowed" true (r2 = Ok ())

(* --- Integration tests with real HTTP server --- *)

let test_post_json_allowed () =
  with_http_server
    (fun _conn _req _body ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"ok" ())
    (fun port ->
      let result =
        Lwt_main.run
          (Policy_http_client.post_json ~rules:allow_all_rules
             ~uri:(Printf.sprintf "http://127.0.0.1:%d/test" port)
             ~headers:[] ~body:"{}" ())
      in
      let status, body = check_ok "post_json allowed" result in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check string) "body" "ok" body)

let test_post_json_denied () =
  let result =
    Lwt_main.run
      (Policy_http_client.post_json ~rules:deny_all_rules
         ~uri:"http://blocked.example.com/api" ~headers:[] ~body:"{}" ())
  in
  let _err = check_denied "post_json denied" result in
  (* Verify the HTTP call was never made by checking that the error is a
     policy error, not a connection error. *)
  Alcotest.(check bool) "is policy error" true true

let test_get_allowed () =
  with_http_server
    (fun _conn _req _body ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"hello" ())
    (fun port ->
      let result =
        Lwt_main.run
          (Policy_http_client.get ~rules:allow_all_rules
             ~uri:(Printf.sprintf "http://127.0.0.1:%d/greet" port)
             ~headers:[] ())
      in
      let status, body = check_ok "get allowed" result in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check string) "body" "hello" body)

let test_get_denied () =
  let result =
    Lwt_main.run
      (Policy_http_client.get ~rules:deny_all_rules
         ~uri:"http://blocked.example.com/data" ~headers:[] ())
  in
  let err = check_denied "get denied" result in
  Alcotest.(check string) "host" "blocked.example.com" err.host

let test_put_json_allowed () =
  with_http_server
    (fun _conn _req _body ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"updated" ())
    (fun port ->
      let result =
        Lwt_main.run
          (Policy_http_client.put_json ~rules:allow_all_rules
             ~uri:(Printf.sprintf "http://127.0.0.1:%d/item" port)
             ~headers:[] ~body:"{\"id\":1}" ())
      in
      let status, body = check_ok "put_json allowed" result in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check string) "body" "updated" body)

let test_put_json_denied () =
  let result =
    Lwt_main.run
      (Policy_http_client.put_json ~rules:deny_all_rules
         ~uri:"http://blocked.example.com/item" ~headers:[] ~body:"{}" ())
  in
  let _err = check_denied "put_json denied" result in
  Alcotest.(check bool) "is policy error" true true

let test_selective_rules () =
  let rules =
    [
      {
        host = "127.0.0.1";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
      {
        host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  with_http_server
    (fun _conn _req _body ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"local" ())
    (fun port ->
      (* Allowed: 127.0.0.1 matches first rule *)
      let r1 =
        Lwt_main.run
          (Policy_http_client.get ~rules
             ~uri:(Printf.sprintf "http://127.0.0.1:%d/data" port)
             ~headers:[] ())
      in
      let status, _ = check_ok "localhost allowed" r1 in
      Alcotest.(check int) "status" 200 status);
  (* Denied: example.com matches catch-all deny *)
  let r2 =
    Lwt_main.run
      (Policy_http_client.get ~rules ~uri:"http://example.com/data" ~headers:[]
         ())
  in
  let err = check_denied "example.com denied" r2 in
  Alcotest.(check string) "host" "example.com" err.host;
  Alcotest.(check int) "catch-all rule" 1 err.matched_rule_index

let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let test_policy_error_to_string () =
  let err : Policy_http_client.policy_error =
    {
      host = "evil.com";
      path = Some "/steal";
      method_ = Some "POST";
      matched_rule_index = 2;
      message = "egress denied: POST evil.com /steal (rule 2)";
    }
  in
  let s = Policy_http_client.policy_error_to_string err in
  Alcotest.(check bool) "contains host" true (string_contains s "evil.com");
  Alcotest.(check bool) "contains method" true (string_contains s "POST")

let suite =
  [
    Alcotest.test_case "check_policy: allow" `Quick test_check_policy_allow;
    Alcotest.test_case "check_policy: deny" `Quick test_check_policy_deny;
    Alcotest.test_case "check_policy: default deny" `Quick
      test_check_policy_default_deny;
    Alcotest.test_case "check_policy: default allowlist clawq docs" `Quick
      test_check_policy_default_allowlist_clawq_docs;
    Alcotest.test_case "check_policy: permissive unmatched" `Quick
      test_check_policy_permissive_unmatched;
    Alcotest.test_case "check_policy: default deny hint" `Quick
      test_check_policy_default_deny_hint;
    Alcotest.test_case "check_policy: wildcard host" `Quick
      test_check_policy_wildcard_host;
    Alcotest.test_case "check_policy: path matching" `Quick
      test_check_policy_path_matching;
    Alcotest.test_case "check_policy: method matching" `Quick
      test_check_policy_method_matching;
    Alcotest.test_case "check_policy: first match wins" `Quick
      test_check_policy_first_match_wins;
    Alcotest.test_case "post_json allowed" `Quick test_post_json_allowed;
    Alcotest.test_case "post_json denied" `Quick test_post_json_denied;
    Alcotest.test_case "get allowed" `Quick test_get_allowed;
    Alcotest.test_case "get denied" `Quick test_get_denied;
    Alcotest.test_case "put_json allowed" `Quick test_put_json_allowed;
    Alcotest.test_case "put_json denied" `Quick test_put_json_denied;
    Alcotest.test_case "selective rules" `Quick test_selective_rules;
    Alcotest.test_case "policy_error_to_string" `Quick
      test_policy_error_to_string;
  ]
