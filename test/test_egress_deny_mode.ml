(* Tests for egress deny mode integration with GitHub API and MCP paths.

   Verifies that:
   - GitHub API URLs are correctly evaluated against egress rules
   - MCP server URLs are correctly evaluated against egress rules
   - Policy_http_client.check_policy works with realistic URLs
   - Deny decisions are returned for blocked hosts/paths *)

open Runtime_config

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

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

let parse_config json =
  Config_loader.parse_config (Yojson.Safe.from_string json)

(** Helper: create a rule that allows a host *)
let allow_rule ?path ?method_ host =
  { host; path; method_; action = Allow; log_policy = No_log }

(** Helper: create a rule that denies a host *)
let deny_rule ?path ?method_ host =
  { host; path; method_; action = Deny; log_policy = Log }

let config_with_default_egress_rule rule =
  {
    Runtime_config.default with
    egress = { strictness = Strict; default_allowlist = [ rule ] };
  }

(** Test: GitHub API calls are denied by catch-all deny rule *)
let test_github_api_denied_by_default () =
  let rules = [ deny_rule "*" ] in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/issues/1/comments"
      ~method_:"POST" ()
  in
  match result with
  | Ok () -> Alcotest.fail "expected deny for GitHub API with catch-all rule"
  | Error err ->
      Alcotest.check Alcotest.string "host" "api.github.com" err.host;
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "method" (Some "POST") err.method_

(** Test: GitHub API calls are allowed when explicitly permitted *)
let test_github_api_allowed_by_rule () =
  let rules = [ allow_rule "api.github.com"; deny_rule "*" ] in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/issues/1/comments"
      ~method_:"POST" ()
  in
  match result with
  | Ok () -> ()
  | Error err -> Alcotest.failf "expected allow, got deny: %s" err.message

(** Test: GitHub API calls denied for specific path *)
let test_github_api_path_denied () =
  let rules =
    [
      deny_rule ~path:"/repos/owner/admin" "api.github.com";
      allow_rule "api.github.com";
      deny_rule "*";
    ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/admin" ~method_:"POST" ()
  in
  (match r1 with
  | Ok () -> Alcotest.fail "expected deny for admin path"
  | Error _ -> ());
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/issues" ~method_:"GET" ()
  in
  match r2 with
  | Ok () -> ()
  | Error err ->
      Alcotest.failf "expected allow for non-admin path: %s" err.message

(** Test: MCP HTTP server URL is denied by egress policy *)
let test_mcp_http_server_denied () =
  let rules = [ deny_rule "mcp.evil.com" ] in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"https://mcp.evil.com/tools/call" ~method_:"POST" ()
  in
  match result with
  | Ok () -> Alcotest.fail "expected deny for MCP server URL"
  | Error err -> Alcotest.check Alcotest.string "host" "mcp.evil.com" err.host

(** Test: MCP HTTP server URL is allowed when whitelisted *)
let test_mcp_http_server_allowed () =
  let rules = [ allow_rule "mcp.trusted.com"; deny_rule "*" ] in
  let result =
    Policy_http_client.check_policy ~rules
      ~uri:"https://mcp.trusted.com/tools/call" ~method_:"POST" ()
  in
  match result with
  | Ok () -> ()
  | Error err ->
      Alcotest.failf "expected allow for trusted MCP server: %s" err.message

(** Test: Wildcard subdomain matching for GitHub API *)
let test_github_wildcard_subdomain () =
  let rules = [ allow_rule "*.github.com"; deny_rule "*" ] in
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo" ()
  in
  (match r1 with
  | Ok () -> ()
  | Error err ->
      Alcotest.failf "expected allow for api.github.com: %s" err.message);
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://raw.github.com/owner/repo/file.txt" ()
  in
  match r2 with
  | Ok () -> ()
  | Error err ->
      Alcotest.failf "expected allow for raw.github.com: %s" err.message

(** Test: Deny all external but allow specific GitHub API endpoints *)
let test_selective_github_egress () =
  let rules =
    [
      allow_rule ~path:"/repos/owner/repo/issues/*" ~method_:"POST"
        "api.github.com";
      allow_rule ~path:"/repos/owner/repo/pulls/*" ~method_:"GET"
        "api.github.com";
      deny_rule "api.github.com";
      deny_rule "*";
    ]
  in
  (* Allowed: POST to issues *)
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/issues/1/comments"
      ~method_:"POST" ()
  in
  (match r1 with
  | Ok () -> ()
  | Error err -> Alcotest.failf "expected allow for POST issues: %s" err.message);
  (* Denied: POST to pulls *)
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/pulls/1/comments"
      ~method_:"POST" ()
  in
  (match r2 with
  | Ok () -> Alcotest.fail "expected deny for POST pulls"
  | Error _ -> ());
  (* Allowed: GET to pulls *)
  let r3 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo/pulls/1/files"
      ~method_:"GET" ()
  in
  match r3 with
  | Ok () -> ()
  | Error err -> Alcotest.failf "expected allow for GET pulls: %s" err.message

(** Test: Empty rules deny all (safe default) *)
let test_empty_rules_deny_all () =
  let result =
    Policy_http_client.check_policy ~rules:[]
      ~uri:"https://api.github.com/repos/owner/repo" ()
  in
  match result with
  | Ok () -> Alcotest.fail "expected deny with empty rules"
  | Error _ -> ()

let test_web_search_denied_before_http () =
  let config =
    {
      Runtime_config.default with
      web_search =
        Some
          {
            search_provider = "ddg";
            search_api_key = "";
            num_results = 1;
            search_base_url = Some "https://blocked.example.test";
            credential_handle = None;
          };
    }
  in
  let tool = Tools_builtin_net.web_search ~config in
  let context =
    { Tool.default_context with egress_rules = [ deny_rule "*" ] }
  in
  let result =
    Lwt_main.run
      (tool.Tool.invoke ~context
         (`Assoc [ ("query", `String "clawq egress policy") ]))
  in
  Alcotest.(check bool)
    "web_search reports policy denial" true
    (Test_helpers.string_contains result "egress denied")

let test_no_context_http_get_uses_loaded_config () =
  let config =
    config_with_default_egress_rule (deny_rule "blocked.example.test")
  in
  let tool = Tools_builtin.http_get ~config ~workspace_only:false in
  let result =
    Lwt_main.run
      (tool.Tool.invoke
         (`Assoc [ ("url", `String "https://blocked.example.test/path") ]))
  in
  Alcotest.(check bool)
    "http_get no-context deny uses configured fallback rule" true
    (Test_helpers.string_contains result "(rule 0)")

let test_no_context_web_search_uses_loaded_config () =
  let config =
    {
      (config_with_default_egress_rule (deny_rule "blocked.example.test")) with
      web_search =
        Some
          {
            search_provider = "ddg";
            search_api_key = "";
            num_results = 1;
            search_base_url = Some "https://blocked.example.test";
            credential_handle = None;
          };
    }
  in
  let tool = Tools_builtin.web_search ~config in
  let result =
    Lwt_main.run
      (tool.Tool.invoke (`Assoc [ ("query", `String "clawq egress policy") ]))
  in
  Alcotest.(check bool)
    "web_search no-context deny uses configured fallback rule" true
    (Test_helpers.string_contains result "(rule 0)")

(** Test: GitHub API wrapper denies before outbound HTTP *)
let test_github_api_wrapper_denies_before_http () =
  let requests = ref 0 in
  with_http_server
    (fun _conn _req _body ->
      incr requests;
      Cohttp_lwt_unix.Server.respond_string ~status:`Created
        ~body:{|{"id":123}|} ())
    (fun port ->
      with_env "CLAWQ_GITHUB_API_BASE"
        (Some (Printf.sprintf "http://127.0.0.1:%d" port))
        (fun () ->
          Lwt_main.run
            (Github_api.post_comment
               ~app_token:(None : Github_app_token.t option)
               ~auth:(GithubPat "test-token")
               ~egress_rules:[ deny_rule "*" ]
               ~owner:"owner" ~repo:"repo" ~issue_number:1 ~body:"blocked" ());
          Alcotest.(check int) "no outbound request" 0 !requests))

(** Test: Empty GitHub API rules preserve legacy raw HTTP behavior *)
let test_github_api_empty_rules_allows_legacy_http () =
  let requests = ref 0 in
  with_http_server
    (fun _conn _req _body ->
      incr requests;
      Cohttp_lwt_unix.Server.respond_string ~status:`Created
        ~body:{|{"id":123}|} ())
    (fun port ->
      with_env "CLAWQ_GITHUB_API_BASE"
        (Some (Printf.sprintf "http://127.0.0.1:%d" port))
        (fun () ->
          Lwt_main.run
            (Github_api.post_comment
               ~app_token:(None : Github_app_token.t option)
               ~auth:(GithubPat "test-token") ~egress_rules:[] ~owner:"owner"
               ~repo:"repo" ~issue_number:1 ~body:"allowed" ());
          Alcotest.(check int) "one outbound request" 1 !requests))

(** Test: MCP connect_with_policy denies before startup handshake HTTP *)
let test_mcp_startup_denied_before_http () =
  let requests = ref 0 in
  let config =
    parse_config
      {|{
        "access_bundles": [
          {"id": "deny-net", "egress_rules": [
            {"host": "*", "action": "deny", "log_policy": "log"}
          ]}
        ],
        "access_scopes": [
          {"id": "default", "level": "default", "access_bundle_ids": ["deny-net"]}
        ]
      }|}
  in
  let snapshot =
    Access_snapshot.create ~config ~work_type:Access_snapshot.Background_task ()
  in
  let cfg =
    {
      Mcp_client.name = "blocked-mcp";
      command = "https://blocked.example.test/rpc";
      args = [];
      env = [];
      credential_handle = None;
    }
  in
  let fake_post ~url:_ ~headers:_ ~body:_ =
    incr requests;
    Lwt.return (200, "{}", "application/json")
  in
  let result =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           let open Lwt.Syntax in
           let* _client =
             Mcp_client.connect_with_policy ~config ~snapshot
               ~http_post:fake_post cfg
           in
           Lwt.return (Ok ()))
         (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected MCP egress denial"
  | Error msg ->
      Alcotest.(check bool)
        "message mentions egress denial" true
        (Test_helpers.string_contains msg "egress denied"));
  Alcotest.(check int) "no startup request" 0 !requests

(** Test: Empty MCP egress rules still use the top-level strict default. *)
let test_mcp_startup_empty_rules_uses_strict_default () =
  let requests = ref 0 in
  let config = Runtime_config.default in
  let snapshot =
    Access_snapshot.create ~config ~work_type:Access_snapshot.Background_task ()
  in
  let cfg =
    {
      Mcp_client.name = "blocked-mcp";
      command = "https://blocked.example.test/rpc";
      args = [];
      env = [];
      credential_handle = None;
    }
  in
  let fake_post ~url:_ ~headers:_ ~body:_ =
    incr requests;
    Lwt.return (200, "{}", "application/json")
  in
  let result =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           let open Lwt.Syntax in
           let* _client =
             Mcp_client.connect_with_policy ~config ~snapshot
               ~http_post:fake_post cfg
           in
           Lwt.return (Ok ()))
         (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected MCP egress denial"
  | Error msg ->
      Alcotest.(check bool)
        "message mentions egress denial" true
        (Test_helpers.string_contains msg "egress denied"));
  Alcotest.(check int) "no startup request" 0 !requests

(** Test: Deny specific MCP server but allow GitHub *)
let test_mcp_denied_github_allowed () =
  let rules =
    [ allow_rule "api.github.com"; deny_rule "mcp.evil.com"; deny_rule "*" ]
  in
  let r1 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://api.github.com/repos/owner/repo" ()
  in
  (match r1 with
  | Ok () -> ()
  | Error err -> Alcotest.failf "expected allow for GitHub: %s" err.message);
  let r2 =
    Policy_http_client.check_policy ~rules
      ~uri:"https://mcp.evil.com/tools/call" ~method_:"POST" ()
  in
  match r2 with
  | Ok () -> Alcotest.fail "expected deny for evil MCP server"
  | Error _ -> ()

let suite =
  [
    Alcotest.test_case "GitHub API denied by catch-all rule" `Quick
      test_github_api_denied_by_default;
    Alcotest.test_case "GitHub API allowed by explicit rule" `Quick
      test_github_api_allowed_by_rule;
    Alcotest.test_case "GitHub API path denied" `Quick
      test_github_api_path_denied;
    Alcotest.test_case "MCP HTTP server denied" `Quick
      test_mcp_http_server_denied;
    Alcotest.test_case "MCP HTTP server allowed" `Quick
      test_mcp_http_server_allowed;
    Alcotest.test_case "GitHub wildcard subdomain" `Quick
      test_github_wildcard_subdomain;
    Alcotest.test_case "Selective GitHub egress" `Quick
      test_selective_github_egress;
    Alcotest.test_case "Empty rules deny all" `Quick test_empty_rules_deny_all;
    Alcotest.test_case "web_search denied before HTTP" `Quick
      test_web_search_denied_before_http;
    Alcotest.test_case "no-context http_get uses loaded config" `Quick
      test_no_context_http_get_uses_loaded_config;
    Alcotest.test_case "no-context web_search uses loaded config" `Quick
      test_no_context_web_search_uses_loaded_config;
    Alcotest.test_case "GitHub wrapper denies before HTTP" `Quick
      test_github_api_wrapper_denies_before_http;
    Alcotest.test_case "GitHub empty rules preserve legacy HTTP" `Quick
      test_github_api_empty_rules_allows_legacy_http;
    Alcotest.test_case "MCP startup denied before HTTP" `Quick
      test_mcp_startup_denied_before_http;
    Alcotest.test_case "MCP empty rules use strict default" `Quick
      test_mcp_startup_empty_rules_uses_strict_default;
    Alcotest.test_case "MCP denied, GitHub allowed" `Quick
      test_mcp_denied_github_allowed;
  ]
