let provider ?base_url ?(api_key = "ocx_test") () :
    Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    kind = Some Opencodex.provider_kind;
    base_url;
    api_key;
  }

let test_contract_constants () =
  Alcotest.(check string)
    "default endpoint" "http://cachy.lan:10100/v1" Opencodex.default_base_url;
  Alcotest.(check string)
    "IP endpoint" "http://10.100.1.2:10100/v1" Opencodex.ip_base_url;
  Alcotest.(check string) "wire API" "responses_http" Opencodex.wire_api;
  Alcotest.(check bool)
    "websockets disabled" false Opencodex.supports_websockets

let test_resolve_token_precedence () =
  let getenv _ = Some "ocx_env" in
  let read_file _ = Some "ocx_file\n" in
  Alcotest.(check (option string))
    "configured first" (Some "ocx_config")
    (Opencodex.resolve_token ~getenv ~read_file ~configured:" ocx_config " ());
  Alcotest.(check (option string))
    "env before file" (Some "ocx_env")
    (Opencodex.resolve_token ~getenv ~read_file ~configured:"" ());
  Alcotest.(check (option string))
    "file fallback" (Some "ocx_file")
    (Opencodex.resolve_token
       ~getenv:(fun _ -> None)
       ~read_file ~configured:"" ())

let test_resolve_token_rejects_wrong_prefix () =
  let token =
    Opencodex.resolve_token
      ~getenv:(fun _ -> Some "sk-not-ocx")
      ~read_file:(fun _ -> Some "also-wrong")
      ~configured:"wrong" ()
  in
  Alcotest.(check (option string)) "invalid tokens rejected" None token

let test_request_metadata () =
  let p = provider ~base_url:"http://10.100.1.2:10100/v1/" () in
  Alcotest.(check string)
    "responses endpoint" "http://10.100.1.2:10100/v1/responses"
    (Opencodex.responses_uri p);
  Alcotest.(check string)
    "models endpoint" "http://10.100.1.2:10100/v1/models"
    (Opencodex.models_uri p);
  Alcotest.(check (list (pair string string)))
    "custom auth only"
    [ ("x-opencodex-api-key", "ocx_test") ]
    (Opencodex.auth_headers p.api_key)

let test_build_body_preserves_upstream_route () =
  let request_model model =
    Provider_openai_codex.build_body ~provider_name:"opencodex" ~model
      ~messages:[] ~provider:(provider ()) None
    |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "model"
    |> Yojson.Safe.Util.to_string
  in
  Alcotest.(check string)
    "strips only outer OpenCodex prefix" "xai/grok-4"
    (request_model "opencodex:xai/grok-4");
  Alcotest.(check string)
    "preserves selected upstream route" "xai/grok-4"
    (request_model "xai/grok-4")

let test_config_loader_integration () =
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "opencodex": {
            "kind": "opencodex",
            "api_key": "ocx_configured"
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let p = List.assoc "opencodex" cfg.providers in
  Alcotest.(check string) "configured token" "ocx_configured" p.api_key;
  Alcotest.(check string)
    "default base URL" Opencodex.default_base_url
    (Model_discovery.get_base_url ~name:"opencodex" p)

let test_routing_kind_and_auth () =
  let p = provider () in
  Alcotest.(check bool)
    "detected kind" true
    (Provider.detect_kind ~name:"opencodex" p = Provider.OpenCodex);
  Alcotest.(check bool)
    "routable valid token" true
    (Provider.provider_has_routable_auth ~name:"opencodex" p);
  let invalid = { p with api_key = "sk-wrong" } in
  Alcotest.(check bool)
    "invalid token not routable" false
    (Provider.provider_has_routable_auth ~name:"opencodex" invalid)

let test_model_discovery_custom_auth_header () =
  Alcotest.(check (list (pair string string)))
    "OpenCodex models auth"
    [ ("x-opencodex-api-key", "ocx_test") ]
    (Model_discovery.openai_model_auth_headers
       ~auth_header:Opencodex.api_key_header ~api_key:"ocx_test" ())

let test_model_discovery_uri_trims_trailing_slash () =
  Alcotest.(check string)
    "OpenCodex models URI" "http://10.100.1.2:10100/v1/models"
    (Model_discovery.models_uri ~provider_name:"opencodex"
       (provider ~base_url:"http://10.100.1.2:10100/v1/" ()))

let test_debug_header_redacted () =
  let headers =
    Http_debug.redact_headers
      [ ("x-opencodex-api-key", "ocx_super_secret_token") ]
  in
  match headers with
  | [ (_, value) ] ->
      Alcotest.(check bool)
        "raw token absent" false
        (Test_helpers.string_contains value "ocx_super_secret_token")
  | _ -> Alcotest.fail "expected one redacted header"

let test_catalog_default () =
  match Models_catalog.find_by_full_name "opencodex:gpt-5.4" with
  | Some model ->
      Alcotest.(check string)
        "catalog provider" "opencodex" model.Models_catalog.provider
  | None -> Alcotest.fail "expected OpenCodex default model in catalog"

let test_doctor_warning_is_actionable () =
  let warnings =
    Opencodex.doctor_warnings ~name:"opencodex" (provider ~api_key:"" ())
  in
  match warnings with
  | [ warning ] ->
      Alcotest.(check bool)
        "names env var" true
        (Test_helpers.string_contains warning Opencodex.api_auth_token_env);
      Alcotest.(check bool)
        "names token file" true
        (Test_helpers.string_contains warning ".opencodex/api-token")
  | _ -> Alcotest.fail "expected one OpenCodex doctor warning"

let suite =
  [
    Alcotest.test_case "contract constants" `Quick test_contract_constants;
    Alcotest.test_case "token precedence" `Quick test_resolve_token_precedence;
    Alcotest.test_case "token prefix validation" `Quick
      test_resolve_token_rejects_wrong_prefix;
    Alcotest.test_case "request metadata" `Quick test_request_metadata;
    Alcotest.test_case "request body preserves upstream route" `Quick
      test_build_body_preserves_upstream_route;
    Alcotest.test_case "config loader integration" `Quick
      test_config_loader_integration;
    Alcotest.test_case "routing kind and auth" `Quick test_routing_kind_and_auth;
    Alcotest.test_case "model discovery auth header" `Quick
      test_model_discovery_custom_auth_header;
    Alcotest.test_case "model discovery URI" `Quick
      test_model_discovery_uri_trims_trailing_slash;
    Alcotest.test_case "debug header redacted" `Quick test_debug_header_redacted;
    Alcotest.test_case "catalog default" `Quick test_catalog_default;
    Alcotest.test_case "doctor warning" `Quick test_doctor_warning_is_actionable;
  ]
