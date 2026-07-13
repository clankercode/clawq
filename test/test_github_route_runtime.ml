(** Production seams for GitHub route/App setup: the full CLI and the verified
    callback continuation. These tests deliberately avoid direct apply-only
    helpers so regressions cannot leave the acceptance path test-only. *)

module Callback = Github_app_setup_callback
module Runtime = Github_app_setup_runtime
module Ingress = Github_app_setup_ingress
module Store = Github_route_store
module Filter = Github_route_filter
module Envelope = Github_event_envelope

let fixed_now = 1_700_000_000.

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Github_app_setup_tx.ensure_schema db;
  Callback.ensure_schema db;
  Store.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let authenticated_actor principal_id : Setup_plan_consent.actor =
  { principal_id; role = Global_admin; source_room_id = None }

let cli_actor = authenticated_actor "cli:github-route"

let cli_cmd ?(actor = cli_actor) ~db ~config args =
  Github_route_cli.cmd_with_db ~actor ~db ~config args

let assert_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail error

let text_column stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT value -> value | _ -> ""

let count_sql db sql =
  let stmt = Sqlite3.prepare db sql in
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT value -> Int64.to_int value
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  result

let latest_plan db =
  let stmt =
    Sqlite3.prepare db
      "SELECT id FROM setup_plans ORDER BY created_at DESC, id DESC LIMIT 1"
  in
  let id =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> text_column stmt 0
    | _ -> Alcotest.fail "expected stored setup plan"
  in
  ignore (Sqlite3.finalize stmt);
  match Setup_plan_apply.get_plan ~db ~plan_id:id with
  | Some plan -> plan
  | None -> Alcotest.fail "stored setup plan could not be loaded"

let test_cli_plan_then_explicit_apply () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  let planned =
    cli_cmd ~db ~config
      [
        "route";
        "plan";
        "room-runtime";
        "repo:Acme/Widget";
        "--id";
        "rt-runtime";
      ]
  in
  Alcotest.(check bool)
    "CLI rendered confirmation digest" true
    (Test_helpers.string_contains planned "Digest:");
  let plan = latest_plan db in
  let applied =
    cli_cmd ~db ~config [ "route"; "apply"; plan.id; plan.digest ]
  in
  Alcotest.(check bool)
    "CLI applied only after explicit digest" true
    (Test_helpers.string_contains applied "Applied plan");
  Alcotest.(check int)
    "route materialized" 1
    (count_sql db "SELECT COUNT(*) FROM github_routes WHERE id = 'rt-runtime'");
  Alcotest.(check bool)
    "durable correlated apply audit" true
    (List.exists
       (fun (event : Github_route_ops.audit_record) ->
         event.action = "setup_plan_applied"
         && event.setup_plan_id = Some plan.id)
       (Github_route_ops.list_audit ~db ~setup_plan_id:plan.id ()))

let test_cli_typed_filter_plan_and_preview () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  let filter_json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("schema_version", `Int 1);
           ( "pr",
             `Assoc
               [
                 ( "labels",
                   `Assoc
                     [
                       ("op", `String "in");
                       ("values", `List [ `String "ready" ]);
                     ] );
               ] );
         ])
  in
  let planned =
    cli_cmd ~db ~config
      [
        "route";
        "plan";
        "room-filter-runtime";
        "repo:Acme/Widget";
        "--id";
        "rt-filter-runtime";
        "--filter-json";
        filter_json;
      ]
  in
  Alcotest.(check bool)
    "typed filter JSON produces a plan" true
    (Test_helpers.string_contains planned "Digest:");
  let plan = latest_plan db in
  let applied =
    cli_cmd ~db ~config [ "route"; "apply"; plan.id; plan.digest ]
  in
  Alcotest.(check bool)
    "typed filter plan applies" true
    (Test_helpers.string_contains applied "Applied plan");
  let route =
    match Store.get ~db ~id:"rt-filter-runtime" with
    | Ok (Some route) -> route
    | Ok None -> Alcotest.fail "typed filter route was not persisted"
    | Error error -> Alcotest.fail error
  in
  Alcotest.(check bool)
    "persisted route retains typed advanced filter" true
    (Filter.has_advanced route.filter);
  let envelope : Envelope.t =
    {
      version = Envelope.envelope_version;
      delivery_id = Some "cli-filter-preview";
      installation_id = Some 1;
      event = "pull_request";
      action = Some "opened";
      repo_full_name = "acme/widget";
      org = Some "acme";
      item_kind = Some Envelope.Pull_request;
      item_number = Some 9;
      item_node_id = Some "PR_cli_filter_preview";
      item_url = None;
      html_url = None;
      family = Envelope.Lifecycle;
      actor = { Envelope.empty_actor with login = Some "webhook-sender" };
      item_author = Some "item-author";
      before = None;
      after = Some { Envelope.empty_safe_state with labels = [ "ready" ] };
      transfer = None;
      received_at = None;
      event_at = None;
      head_sha = None;
      unsupported = false;
      skip_reason = None;
    }
  in
  let preview =
    cli_cmd ~db ~config
      [
        "route";
        "preview";
        "room-filter-runtime";
        "--envelope-json";
        Yojson.Safe.to_string (Envelope.to_safe_json envelope);
      ]
  in
  Alcotest.(check bool)
    "preview matches typed filter" true
    (Test_helpers.string_contains preview "decision=Matched");
  Alcotest.(check bool)
    "preview explains typed predicate" true
    (Test_helpers.string_contains preview "pr.labels PASS")

let test_minimal_build_disables_github_surface () =
  let result = Command_bridge_min.handle [ "github"; "route"; "list" ] in
  Alcotest.(check bool)
    "minimal build reports full-build boundary" true
    (Test_helpers.string_contains result "not available in the minimal build")

let test_cli_rechecks_independent_principal () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  let planned =
    cli_cmd ~db ~config
      [ "route"; "plan"; "room-principal"; "repo:Acme/Principal" ]
  in
  Alcotest.(check bool)
    "plan was accepted for CLI principal" true
    (Test_helpers.string_contains planned "Digest:");
  let plan = latest_plan db in
  let outcome =
    cli_cmd
      ~actor:(authenticated_actor "principal:other")
      ~db ~config
      [ "route"; "apply"; plan.id; plan.digest ]
  in
  Alcotest.(check bool)
    "different runtime principal is rejected" true
    (Test_helpers.string_contains outcome "principal_mismatch");
  Alcotest.(check int)
    "mismatched principal made no route" 0
    (count_sql db "SELECT COUNT(*) FROM github_routes")

let test_cli_route_diagnostics_export_and_validate () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  ignore
    (assert_ok
       (Store.create ~db ~id:"rt-live-report"
          ~destination:(Store.Room "room-live-report")
          ~selector:(Store.Repo "acme/widget") ~now:fixed_now ()));
  let diagnostics =
    cli_cmd ~db ~config
      [ "route"; "diagnostics"; "--room"; "room-live-report"; "--json" ]
  in
  Alcotest.(check bool)
    "read-only diagnostics emit JSON without an actor" true
    (Test_helpers.string_contains diagnostics "\"routes\"");
  Alcotest.(check bool)
    "diagnostics does not leak private configuration" false
    (Test_helpers.string_contains
       (String.lowercase_ascii diagnostics)
       "private_key");
  let export =
    cli_cmd ~db ~config [ "route"; "export"; "--room"; "room-live-report" ]
  in
  Alcotest.(check bool)
    "export is JSON" true
    (Test_helpers.string_contains export "\"current_filter_schema_version\"");
  let envelope_json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("version", `Int Envelope.envelope_version);
           ("event", `String "pull_request");
           ("repo_full_name", `String "acme/widget");
           ("item_kind", `String "pull_request");
           ("item_number", `Int 17);
           ("head_sha", `String "diagnostics-must-not-echo-this-sha");
         ])
  in
  let text_explain =
    cli_cmd ~db ~config
      [
        "route";
        "diagnostics";
        "--room";
        "room-live-report";
        "--envelope-json";
        envelope_json;
      ]
  in
  Alcotest.(check bool)
    "diagnostics text includes matching winner and decision" true
    (Test_helpers.string_contains text_explain
       "winning_selector=repo:acme/widget"
    && Test_helpers.string_contains text_explain "decision=Matched"
    && Test_helpers.string_contains text_explain "final_reason="
    && Test_helpers.string_contains text_explain "predicate:"
    && Test_helpers.string_contains text_explain "enrichment:");
  Alcotest.(check bool)
    "diagnostics text never echoes envelope-only SHA" false
    (Test_helpers.string_contains text_explain
       "diagnostics-must-not-echo-this-sha");
  let json_explain =
    cli_cmd ~db ~config
      [
        "route";
        "export";
        "--room";
        "room-live-report";
        "--envelope-json";
        envelope_json;
      ]
  in
  Alcotest.(check bool)
    "export JSON includes safe explain fields" true
    (Test_helpers.string_contains json_explain "\"winning_selector\""
    && Test_helpers.string_contains json_explain "\"decision\""
    && Test_helpers.string_contains json_explain "\"final_reason\""
    && Test_helpers.string_contains json_explain "\"predicate_reasons\""
    && Test_helpers.string_contains json_explain "\"enrichment_status\""
    && Test_helpers.string_contains json_explain "paths:not_demanded");
  Alcotest.(check bool)
    "export JSON never echoes envelope-only SHA" false
    (Test_helpers.string_contains json_explain
       "diagnostics-must-not-echo-this-sha");
  let missing_room =
    cli_cmd ~db ~config
      [ "route"; "diagnostics"; "--envelope-json"; envelope_json ]
  in
  Alcotest.(check bool)
    "diagnostics explain requires Room" true
    (Test_helpers.string_contains missing_room "requires --room ROOM");
  let malformed_room =
    cli_cmd ~db ~config
      [ "route"; "export"; "--room"; "--envelope-json"; envelope_json ]
  in
  Alcotest.(check bool)
    "diagnostics explain rejects missing Room value" true
    (Test_helpers.string_contains malformed_room "--room requires");
  let malformed_envelope =
    cli_cmd ~db ~config
      [
        "route";
        "export";
        "--room";
        "room-live-report";
        "--envelope-json";
        "{not-json";
      ]
  in
  Alcotest.(check bool)
    "diagnostics explain rejects malformed safe envelope" true
    (Test_helpers.string_contains malformed_envelope
       "invalid --envelope-json JSON");
  ignore
    (assert_ok
       (Github_route_ops.request_catalog_refresh ~db ~setup_plan_id:"plan-live"
          ~room_id:"room-live-report" ()));
  let validation =
    cli_cmd ~db ~config [ "route"; "validate"; "--room"; "room-live-report" ]
  in
  Alcotest.(check bool)
    "validation reads pending refresh queue" true
    (Test_helpers.string_contains validation "room-live-report");
  Alcotest.(check bool)
    "validation reports unavailable Room catalog, MCP, and session probes as \
     warnings"
    true
    (Test_helpers.string_contains validation "catalog_state_unavailable"
    && Test_helpers.string_contains validation "tools_catalog"
    && Test_helpers.string_contains validation "mcp_catalog"
    && Test_helpers.string_contains validation "session_refresh_no_restart"
    && Test_helpers.string_contains validation "warn")

let principal =
  Github_app_setup_tx.
    { id = "principal:runtime"; kind = "principal"; label = Some "Runtime" }

(* A generated test-only RSA key.  The HTTP ingress signs an App JWT before it
   requests the authenticated installation scope, so this fixture must be
   parseable rather than the intentionally fake PEM used by callback-only
   tests. *)
let test_app_private_key =
  "-----BEGIN PRIVATE KEY-----\n\
   MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQCsnhf1HPJ2Jj7A\n\
   PnCwfndw3SXdlc3prIImQF5fYmN1CYUO3BPDuHRRegRytevmPw/pvxj8HebqJyZl\n\
   WU5dBrh5tEyvfyJIByM9Oh0vAFcw9SJkXGi5tG3cI8BMucdRLOfXUH88hE/bnxFn\n\
   r1kImPfEJSYQ0mOvuPC1DUECQhc9F44fyoTtJVDXipFI1h9WkDOIk23biApyEXXx\n\
   GAgiyCNnzCy5QGt9tM3CQ0qqr56NW66GZd51/baiAUT4YlQPdRHDQXdhhq9DH8ay\n\
   QO8elA7pGIb+Sw33o/Z16/7XWhzEWEFjLZsJ9kyAjqZ/C6/Rt+OiDih7nQE1Rc2U\n\
   KS/f5O/3AgMBAAECggEAA5tsIb2gnXJwJkFHxpBl+5BLfcVnH6Zws87tie266VOx\n\
   GZ3ktdbRa3ByzljZ3J5dvUM2iPIxBJyb00tZ9VyyFyz620H7W+j2Rg3EVVqa99Vv\n\
   igxaTeMk1pBSsOfC8AHRuHCgsAmNx6ebzABgimrz5n/mOzzCQ4ZIVWg4/wyVgrvF\n\
   +pDMeQJLPiHnvqxDsLOao5VOh8LFTmLqhrbZhEOV1BuPshk+jeoDQpOG2IiisowI\n\
   nIh3+nrad7kXYhqALZ7k3ORcis/vEoqtetcsgvI/cKjoimmDn86rpyb2u01rrHt5\n\
   AGAhI0M1vumUixiJaR25Y8MFlj9Vo4hv2umNx5TXsQKBgQDd/Lit4PXwLuLIkCFN\n\
   I9IIdB8VTW+8KUbY1QPAmQH3eC5mEGOjhLnAhwj3DMH1zxJ/NTisSFz9cpNadIl5\n\
   P0Rp/z1MktJBRHuU3RRxIjnehKnCym1PwxFgetVGPfn426FOFWKPBOoaqtkKbOIB\n\
   VIVEX0QSOJDFPRXzW0vC2wi2pwKBgQDHEOK6BBjmI+hkI4jrEPJx5CSxVX4TjyD9\n\
   0kPnP4tFgmBoCxroGEdhPVLsI93p8pU8S9WGHoSZrGF3JKDREfnLam9iMxcmZ74U\n\
   kyiwD3bPfJMuIblI8MFrTPErgtJzvwCCnR7J9ddisaofK0o0xPQX1UPmW4zc8Mn/\n\
   Kzxxks92MQKBgAf9i8w+d7vQhDtB7ODw9CN3wpKqueXk+nbdnAf3ufllaw4jcuK0\n\
   6VbDxY/W9rhZXsoTaVnSNP6ufB1aaoRhwZ2rIVK7SjQtOeGO36h+2eRnlBC95pdj\n\
   ZyG46ipgGrpZdYHxBR4uyBpzoeJdLvlrSGzAnRumy5c97qdW1vBJoBOrAoGADIeO\n\
   jcDGRG4MKYlnC8ykRfDjMlo8NkTzAabjaUHBpV1gbgwM5IDqtT8j4gMb66a+J+5q\n\
   ASgYloeYFuSyTpaAD4Kigh7PHTa4axkcHYDLrKGdrfCndeTZd8R/BYsVbf2erZnw\n\
   HywfI3IlUBLsd8fRyVI+FNi8VAe/3xS8mDVyY3ECgYBitbVyCho4HgPbRWW3l/DK\n\
   O+356CCKEdOcphxoQGE5zZrfq1bJIP8RIRtslcW9mFrddrpKB9SWbY2WIPyvRLFU\n\
   CPB2Nf0DEb+W/AfoaCxoW5LvLh7wSnQvctkPYoLm+A3eDfRJ9mYEsmAaiGQQLgNR\n\
   cTs5Lw5qpm8UVgHR47J5+A==\n\
   -----END PRIVATE KEY-----\n"

let conversion_json =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("id", `Int 424242);
         ("slug", `String "clawq-runtime-test-app");
         ("client_id", `String "Iv1.runtime");
         ("client_secret", `String "cs_test_secret");
         ("pem", `String test_app_private_key);
         ("webhook_secret", `String "whsec_test");
       ])

let verified_scope ~app_id ~installation_id =
  Github_app_installation_scope.with_revision
    {
      installation_id;
      app_id = Some app_id;
      account = { login = "runtime"; id = 1; account_type = "User" };
      selection = Github_app_installation_scope.All_repos;
      repositories = [];
      revoked_repositories = [];
      permissions = [ ("metadata", "read") ];
      status = Github_app_installation_scope.Active;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let exchange_committed ~db ~(tx : Github_app_setup_tx.t) ~installation_id =
  let store_secret ~name ~plaintext:_ = Ok ("handle:" ^ name) in
  let verify_installation ~app_id ~private_key_pem:_ ~installation_id =
    Ok (verified_scope ~app_id ~installation_id)
  in
  assert_ok
    (Callback.exchange ~db
       ~http_post:(fun ~url:_ ~headers:_ ~body:_ -> Ok (201, conversion_json))
       ~verify_installation ~store_secret ~now:fixed_now
       {
         Callback.code = "runtime_code";
         state = tx.state;
         callback_path = Some Github_app_setup_tx.default_callback_path;
         expected_bind = Some tx.bind;
         expected_principal_id = Some principal.id;
         installation_id = Some installation_id;
         setup_action = None;
       })

let test_callback_ingress_authenticates_then_resumes () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  Runtime.install_callback_resume ~db ~current_config:(fun () -> config);
  Fun.protect ~finally:Callback.clear_resume_hook (fun () ->
      let tx =
        assert_ok
          (Github_app_setup_tx.create ~db ~principal
             ~bind:(Github_app_setup_tx.Room "room-runtime")
             ~base_revision:(Setup_plan.base_revision_of_config config)
             ~public_base_url:"https://clawq.example.test" ~app_name:"Clawq"
             ~now:fixed_now ~id:"tx_ingress" ~state:"state_ingress_aaaaaaaa" ())
      in
      let app_installation_json =
        Yojson.Safe.to_string
          (`Assoc
             [
               ("id", `Int 77);
               ("app_id", `Int 424242);
               ("repository_selection", `String "all");
               ( "account",
                 `Assoc
                   [
                     ("login", `String "runtime");
                     ("id", `Int 1);
                     ("type", `String "User");
                   ] );
               ("permissions", `Assoc [ ("metadata", `String "read") ]);
               ("suspended_at", `Null);
             ])
      in
      let client : Ingress.client =
        {
          post =
            (fun ~url ~headers:_ ~body:_ ->
              if String.equal url (Callback.conversion_url ~code:"ingress_code")
              then Lwt.return (Ok (201, conversion_json))
              else Lwt.return (Error "unexpected POST"));
          get =
            (fun ~url ~headers:_ ->
              if String_util.contains url "/app/installations/77" then
                Lwt.return (Ok (200, app_installation_json))
              else Lwt.return (Error "unexpected GET"));
        }
      in
      let store_secret ~name ~plaintext:_ = Ok ("handle:" ^ name) in
      let result =
        Lwt_main.run
          (Ingress.exchange ~db ~client ~code:"ingress_code" ~state:tx.state
             ~installation_id:77 ~store_secret ~now:fixed_now ())
      in
      ignore (assert_ok result);
      Alcotest.(check int)
        "verified HTTP ingress created a durable confirmation delivery" 1
        (List.length (Runtime.list_deliveries ~db ())))

let test_callback_resume_delivery_retries () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  let tx =
    assert_ok
      (Github_app_setup_tx.create ~db ~principal
         ~bind:(Github_app_setup_tx.Room "room-runtime")
         ~base_revision:(Setup_plan.base_revision_of_config config)
         ~public_base_url:"https://clawq.example.test" ~app_name:"Clawq"
         ~now:fixed_now ~id:"tx_retry" ~state:"state_retry_aaaaaaaa" ())
  in
  let exchange = exchange_committed ~db ~tx ~installation_id:78 in
  (match
     Runtime.resume_verified_exchange ~db ~config
       ~persist:(fun ~db:_ _ -> Error "simulated delivery persistence failure")
       exchange
   with
  | Ok () -> Alcotest.fail "expected simulated delivery failure"
  | Error _ -> ());
  Alcotest.(check int)
    "retry retained after delivery failure" 1
    (List.length (Runtime.list_retries ~db ()));
  Alcotest.(check int)
    "retry replays the pending delivery" 1
    (Runtime.retry_pending ~db ~config ());
  Alcotest.(check int)
    "retry queue cleared after delivery" 0
    (List.length (Runtime.list_retries ~db ()));
  Alcotest.(check int)
    "delivery persisted after retry" 1
    (List.length (Runtime.list_deliveries ~db ()))

let test_stale_app_apply_regenerates_and_delivers () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  let newer_config =
    { config with default_temperature = config.default_temperature +. 0.01 }
  in
  let tx =
    assert_ok
      (Github_app_setup_tx.create ~db ~principal
         ~bind:(Github_app_setup_tx.Room "room-runtime")
         ~base_revision:(Setup_plan.base_revision_of_config config)
         ~public_base_url:"https://clawq.example.test" ~app_name:"Clawq"
         ~now:fixed_now ~id:"tx_stale" ~state:"state_stale_aaaaaaaa" ())
  in
  let exchange = exchange_committed ~db ~tx ~installation_id:79 in
  ignore (assert_ok (Runtime.resume_verified_exchange ~db ~config exchange));
  let original_delivery = List.hd (Runtime.list_deliveries ~db ()) in
  let original =
    match Setup_plan_apply.get_plan ~db ~plan_id:original_delivery.plan_id with
    | Some plan -> plan
    | None -> Alcotest.fail "original App setup plan is missing"
  in
  let outcome =
    cli_cmd
      ~actor:(authenticated_actor principal.id)
      ~db ~config:newer_config
      [ "app"; "apply"; original.id; original.digest ]
  in
  Alcotest.(check bool)
    "stale App apply returns replacement" true
    (Test_helpers.string_contains outcome "was stale and was replaced");
  let deliveries = Runtime.list_deliveries ~db () in
  Alcotest.(check int)
    "replacement delivery is durable" 2 (List.length deliveries);
  let replacement_delivery =
    match
      List.find_opt
        (fun (delivery : Runtime.delivery) -> delivery.plan_id <> original.id)
        deliveries
    with
    | Some delivery -> delivery
    | None -> Alcotest.fail "replacement delivery is missing"
  in
  let replacement =
    match
      Setup_plan_apply.get_plan ~db ~plan_id:replacement_delivery.plan_id
    with
    | Some plan -> plan
    | None -> Alcotest.fail "replacement App setup plan is missing"
  in
  Alcotest.(check bool)
    "replacement has a fresh plan id" true
    (replacement.id <> original.id);
  Alcotest.(check string)
    "replacement uses current base revision"
    (Setup_plan.base_revision_of_config newer_config)
    replacement.base_revision

let test_cli_does_not_trust_environment_authority () =
  with_db @@ fun db ->
  let old_admin = Sys.getenv_opt "CLAWQ_ADMIN" in
  let old_principal = Sys.getenv_opt "CLAWQ_PRINCIPAL_ID" in
  Unix.putenv "CLAWQ_ADMIN" "1";
  Unix.putenv "CLAWQ_PRINCIPAL_ID" "principal:forged";
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "CLAWQ_ADMIN" (Option.value old_admin ~default:"");
      Unix.putenv "CLAWQ_PRINCIPAL_ID" (Option.value old_principal ~default:""))
    (fun () ->
      let outcome =
        Github_route_cli.cmd_with_db ~db ~config:Runtime_config.default
          [ "route"; "plan"; "room-runtime"; "repo:acme/widget" ]
      in
      Alcotest.(check bool)
        "environment claims are rejected" true
        (Test_helpers.string_contains outcome "authenticated current actor"));
  Alcotest.(check int)
    "forged environment created no plan" 0
    (count_sql db "SELECT COUNT(*) FROM setup_plans")

let test_verified_callback_resumes_and_never_implicit_applies () =
  with_db @@ fun db ->
  let config = Runtime_config.default in
  Runtime.install_callback_resume ~db ~current_config:(fun () -> config);
  Fun.protect ~finally:Callback.clear_resume_hook (fun () ->
      let tx =
        assert_ok
          (Github_app_setup_tx.create ~db ~principal
             ~bind:(Github_app_setup_tx.Room "room-runtime")
             ~base_revision:(Setup_plan.base_revision_of_config config)
             ~public_base_url:"https://clawq.example.test" ~app_name:"Clawq"
             ~now:fixed_now ~id:"tx_runtime" ~state:"state_runtime_aaaaaaaa" ())
      in
      let store_secret ~name ~plaintext:_ = Ok ("handle:" ^ name) in
      let verify_installation ~app_id ~private_key_pem:_ ~installation_id =
        Ok
          (Github_app_installation_scope.with_revision
             {
               installation_id;
               app_id = Some app_id;
               account = { login = "runtime"; id = 1; account_type = "User" };
               selection = Github_app_installation_scope.All_repos;
               repositories = [];
               revoked_repositories = [];
               permissions = [ ("metadata", "read") ];
               status = Github_app_installation_scope.Active;
               revision = "";
               updated_at = Time_util.iso8601_utc ~t:fixed_now ();
             })
      in
      let request : Callback.exchange_request =
        {
          code = "runtime_code";
          state = tx.state;
          callback_path = Some Github_app_setup_tx.default_callback_path;
          expected_bind = Some tx.bind;
          expected_principal_id = Some principal.id;
          installation_id = Some 99;
          setup_action = None;
        }
      in
      ignore
        (assert_ok
           (Callback.exchange ~db
              ~http_post:(fun ~url:_ ~headers:_ ~body:_ ->
                Ok (201, conversion_json))
              ~verify_installation ~store_secret ~now:fixed_now request));
      let deliveries = Runtime.list_deliveries ~db () in
      Alcotest.(check int)
        "one durable resume delivery" 1 (List.length deliveries);
      let delivery = List.hd deliveries in
      let plan =
        match Setup_plan_apply.get_plan ~db ~plan_id:delivery.plan_id with
        | Some plan -> plan
        | None -> Alcotest.fail "callback delivery did not retain its plan"
      in
      Alcotest.(check bool)
        "callback plan is App setup" true
        (match plan.apply_payload.kind with
        | Setup_plan.Github_app_setup -> true
        | _ -> false);
      Alcotest.(check int)
        "callback did not implicitly apply" 0
        (count_sql db
           "SELECT COUNT(*) FROM setup_plans WHERE status = 'applied'");
      Alcotest.(check bool)
        "callback audit states no apply" true
        (List.exists
           (fun (event : Github_route_ops.audit_record) ->
             event.action = "callback_resumed_confirmable_plan")
           (Github_route_ops.list_audit ~db ~setup_plan_id:plan.id ()));
      let applied =
        cli_cmd
          ~actor:(authenticated_actor principal.id)
          ~db ~config
          [ "app"; "apply"; plan.id; plan.digest ]
      in
      Alcotest.(check bool)
        "App setup requires and accepts explicit apply" true
        (Test_helpers.string_contains applied "Applied plan");
      Alcotest.(check int)
        "App activation persisted" 1
        (count_sql db "SELECT COUNT(*) FROM github_app_setup_activations");
      Alcotest.(check bool)
        "Room App setup attached managed bundle" true
        (Setup_plan_bundle.is_setup_owned ~db ~room_id:"room-runtime"
           ~bundle_id:"github_app_tools" ());
      Alcotest.(check int)
        "App apply durably requests next-turn catalog refresh" 1
        (List.length (Github_route_ops.list_catalog_refresh_requests ~db ())))

let suite =
  [
    ( "CLI route plan and explicit apply",
      `Quick,
      test_cli_plan_then_explicit_apply );
    ( "CLI typed filter plan and read-only preview",
      `Quick,
      test_cli_typed_filter_plan_and_preview );
    ( "minimal build disables GitHub route/App commands",
      `Quick,
      test_minimal_build_disables_github_surface );
    ( "CLI rechecks an independently supplied principal",
      `Quick,
      test_cli_rechecks_independent_principal );
    ( "CLI route diagnostics export and validation are admin protected",
      `Quick,
      test_cli_route_diagnostics_export_and_validate );
    ( "verified callback resumes a confirmable App plan only",
      `Quick,
      test_verified_callback_resumes_and_never_implicit_applies );
    ( "HTTP callback ingress authenticates then resumes",
      `Quick,
      test_callback_ingress_authenticates_then_resumes );
    ( "callback delivery failure is durable and retryable",
      `Quick,
      test_callback_resume_delivery_retries );
    ( "stale App confirmation regenerates and redelivers",
      `Quick,
      test_stale_app_apply_regenerates_and_delivers );
    ( "GitHub route CLI rejects environment authority claims",
      `Quick,
      test_cli_does_not_trust_environment_authority );
  ]
