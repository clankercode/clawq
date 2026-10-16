let provider ?base_url kind =
  { Runtime_config.default_provider_config with kind = Some kind; base_url }

let test_is_provider () =
  Alcotest.(check bool)
    "llmp kind" true
    (Llmp.is_provider ~name:"llmp" (provider "llmp"));
  Alcotest.(check bool)
    "anthropic kind" true
    (Llmp.is_provider ~name:"other" (provider "llmp_anthropic"));
  Alcotest.(check bool)
    "groq" false
    (Llmp.is_provider ~name:"groq" (provider "groq"))

let test_detect_kind () =
  let openai = Provider_routing.detect_kind ~name:"llmp" (provider "llmp") in
  let anthropic =
    Provider_routing.detect_kind ~name:"llmp_anthropic"
      (provider "llmp_anthropic")
  in
  Alcotest.(check bool)
    "openai compat" true
    (openai = Provider_routing.OpenAICompat);
  Alcotest.(check bool) "anthropic" true (anthropic = Provider_routing.Anthropic)

let test_base_urls () =
  Alcotest.(check string)
    "openai default" Llmp.default_base_url
    (Provider_routing.default_base_url_for "llmp");
  Alcotest.(check string)
    "anthropic origin" Llmp.anthropic_base_url
    (Provider_routing.default_base_url_for "llmp_anthropic");
  Alcotest.(check string)
    "mesh openai" Llmp.mesh_base_url
    (Provider_routing.default_base_url_for "llmp_mesh");
  Alcotest.(check string)
    "mesh anthropic" Llmp.mesh_anthropic_base_url
    (Provider_routing.default_base_url_for "llmp_mesh_anthropic");
  Alcotest.(check bool)
    "anthropic base has no /v1 suffix" false
    (String.ends_with ~suffix:"/v1" Llmp.anthropic_base_url)

let test_models_uri () =
  let openai = provider "llmp" in
  let anthropic = provider ~base_url:Llmp.anthropic_base_url "llmp_anthropic" in
  Alcotest.(check string)
    "openai models" "https://omni-dyn-00.amaroolabs.com/v1/models"
    (Model_discovery.models_uri ~provider_name:"llmp" openai);
  Alcotest.(check string)
    "anthropic still uses /v1/models"
    "https://omni-dyn-00.amaroolabs.com/v1/models"
    (Model_discovery.models_uri ~provider_name:"llmp_anthropic" anthropic)

let test_api_model_id () =
  Alcotest.(check string)
    "prefixed" "gpt-6-luna--fast"
    (Llmp.api_model_id "llmp:gpt-6-luna--fast");
  Alcotest.(check string)
    "namespaced" "xai/grok-4.7"
    (Llmp.api_model_id "llmp_anthropic:xai/grok-4.7");
  Alcotest.(check string)
    "bare namespaced" "xai/grok-4.7"
    (Llmp.api_model_id "xai/grok-4.7")

let test_resolve_api_key_keeps_configured () =
  Alcotest.(check string)
    "configured key" "sk-llmp-configured"
    (Llmp.resolve_api_key "sk-llmp-configured")

let with_home dir f =
  let old = Sys.getenv_opt "CLAWQ_HOME" in
  Unix.putenv "CLAWQ_HOME" dir;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some v -> Unix.putenv "CLAWQ_HOME" v
      | None -> Unix.putenv "CLAWQ_HOME" "")
    f

let test_resolve_api_key_from_file () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "clawq-llmp-test" in
  (try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat dir "llmp-api.key" in
  let oc = open_out path in
  output_string oc "sk-llmp-from-file\n";
  close_out oc;
  Llmp.reset_key_cache ();
  with_home dir (fun () ->
      Alcotest.(check string)
        "file key" "sk-llmp-from-file" (Llmp.resolve_api_key "");
      Alcotest.(check bool)
        "file key is routable" true
        (Provider_routing.provider_has_routable_auth ~name:"llmp"
           (provider "llmp")));
  Llmp.reset_key_cache ()

let test_has_routable_auth_empty () =
  Alcotest.(check bool) "empty" false (Llmp.has_routable_auth "")

let test_auth_headers () =
  let anthropic =
    {
      (provider "llmp_anthropic") with
      api_key = "sk-llmp-test";
      base_url = Some Llmp.anthropic_base_url;
    }
  in
  let headers = Provider_anthropic.auth_headers anthropic in
  let names = List.map fst headers in
  Alcotest.(check bool)
    "no authorization" false
    (List.mem "Authorization" names);
  Alcotest.(check bool) "no api-key" false (List.mem "api-key" names);
  Alcotest.(check (option string))
    "x-api-key" (Some "sk-llmp-test")
    (List.assoc_opt "x-api-key" headers);
  let openai = { (provider "llmp") with api_key = "sk-llmp-test" } in
  let bearer = Provider.bearer_headers ~provider_name:"llmp" openai in
  Alcotest.(check (option string))
    "bearer" (Some "Bearer sk-llmp-test")
    (List.assoc_opt "Authorization" bearer);
  Alcotest.(check bool)
    "openai has no x-api-key" false
    (List.mem "x-api-key" (List.map fst bearer))

let test_catalog_models () =
  let models = Models_catalog.by_provider "llmp" in
  Alcotest.(check int) "catalog size" 52 (List.length models);
  List.iter
    (fun name ->
      Alcotest.(check bool)
        name true
        (Option.is_some (Models_catalog.find_by_full_name name)))
    [
      "llmp:gpt-6-luna--fast";
      "llmp:gpt-6.1-sol";
      "llmp:gpt-6.1-sol--fast";
      "llmp:xai/grok-4.7";
      "llmp:grok-imagine-image";
    ];
  match Models_catalog.find_by_full_name "llmp:grok-imagine-image" with
  | None -> Alcotest.fail "missing image model"
  | Some m ->
      Alcotest.(check bool)
        "image model is not a tool model" false m.supports_tools

let test_default_base_url () =
  Alcotest.(check string)
    "llmp default" Llmp.default_base_url
    (Provider_routing.default_base_url_for "llmp")

let suite =
  [
    Alcotest.test_case "is_provider" `Quick test_is_provider;
    Alcotest.test_case "detect_kind" `Quick test_detect_kind;
    Alcotest.test_case "base urls" `Quick test_base_urls;
    Alcotest.test_case "models uri" `Quick test_models_uri;
    Alcotest.test_case "api model id" `Quick test_api_model_id;
    Alcotest.test_case "resolve_api_key keeps configured key" `Quick
      test_resolve_api_key_keeps_configured;
    Alcotest.test_case "resolve_api_key reads key file" `Quick
      test_resolve_api_key_from_file;
    Alcotest.test_case "empty key is not routable" `Quick
      test_has_routable_auth_empty;
    Alcotest.test_case "auth headers stay single-flavour" `Quick
      test_auth_headers;
    Alcotest.test_case "catalog models" `Quick test_catalog_models;
    Alcotest.test_case "default base url" `Quick test_default_base_url;
  ]
