(* B697: Xiaomi MiMo provider data, key resolution, augmentation. *)

let xiaomi_env_vars =
  [
    "XIAOMI_API_KEY";
    "XIAOMI_TOKEN_PLAN_CN_API_KEY";
    "XIAOMI_TOKEN_PLAN_AMS_API_KEY";
    "XIAOMI_TOKEN_PLAN_SGP_API_KEY";
  ]

(* Clear all xiaomi env vars, set the given overrides, run f, restore. Keeps
   env-based tests isolated from each other and from the dev environment. *)
let with_xiaomi_env overrides f =
  let saved = List.map (fun k -> (k, Sys.getenv_opt k)) xiaomi_env_vars in
  List.iter (fun k -> Unix.putenv k "") xiaomi_env_vars;
  List.iter (fun (k, v) -> Unix.putenv k v) overrides;
  Fun.protect f ~finally:(fun () ->
      List.iter
        (fun (k, old) ->
          match old with Some s -> Unix.putenv k s | None -> Unix.putenv k "")
        saved)

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let spec_ids specs = List.map (fun (s : Xiaomi.model_spec) -> s.id) specs

let provider_def name =
  match
    List.find_opt
      (fun (p : Xiaomi.provider_def) -> p.name = name)
      Xiaomi.providers
  with
  | Some p -> p
  | None -> Alcotest.failf "missing provider %s" name

let spec_by_id specs id =
  match List.find_opt (fun (s : Xiaomi.model_spec) -> s.id = id) specs with
  | Some s -> s
  | None -> Alcotest.failf "missing model %s" id

let check_spec label (s : Xiaomi.model_spec) ~context_window ~max_tokens
    ~supports_vision ~input_per_m ~output_per_m ~cache_read_per_m =
  Alcotest.(check int) (label ^ " context") context_window s.context_window;
  Alcotest.(check int) (label ^ " max_tokens") max_tokens s.max_tokens;
  Alcotest.(check bool) (label ^ " vision") supports_vision s.supports_vision;
  Alcotest.(check (float 0.0001)) (label ^ " input") input_per_m s.input_per_m;
  Alcotest.(check (float 0.0001))
    (label ^ " output") output_per_m s.output_per_m;
  Alcotest.(check (float 0.0001))
    (label ^ " cache-read") cache_read_per_m s.cache_read_per_m

let assert_no_provider_json name json =
  let open Yojson.Safe.Util in
  let absent =
    match json |> member "providers" with
    | `Assoc providers -> not (List.mem_assoc name providers)
    | `Null -> true
    | _ -> false
  in
  Alcotest.(check bool) ("provider not serialized: " ^ name) true absent

(* --- resolve_api_key: env-var precedence per region --- *)

let test_resolve_env_per_region () =
  with_xiaomi_env
    [
      ("XIAOMI_API_KEY", "pub-key");
      ("XIAOMI_TOKEN_PLAN_CN_API_KEY", "cn-key");
      ("XIAOMI_TOKEN_PLAN_AMS_API_KEY", "ams-key");
      ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key");
    ]
    (fun () ->
      let check name expected =
        Alcotest.(check (option string))
          name (Some expected)
          (Xiaomi.resolve_api_key name)
      in
      check "xiaomi" "pub-key";
      check "xiaomi-token-plan-cn" "cn-key";
      check "xiaomi-token-plan-ams" "ams-key";
      check "xiaomi-token-plan-sgp" "sgp-key")

let test_provider_defs () =
  let check name base_url env_var =
    let p = provider_def name in
    Alcotest.(check string) (name ^ " base_url") base_url p.base_url;
    Alcotest.(check (list string)) (name ^ " env") [ env_var ] p.env_vars;
    Alcotest.(check (option string))
      (name ^ " default_base_url")
      (Some base_url) (Xiaomi.base_url_for name);
    Alcotest.(check string)
      (name ^ " provider default_base_url")
      base_url
      (Provider.default_base_url_for name)
  in
  Alcotest.(check (list string))
    "provider names"
    [
      "xiaomi";
      "xiaomi-token-plan-cn";
      "xiaomi-token-plan-ams";
      "xiaomi-token-plan-sgp";
    ]
    Xiaomi.provider_names;
  check "xiaomi" "https://api.xiaomimimo.com/anthropic" "XIAOMI_API_KEY";
  check "xiaomi-token-plan-cn" "https://token-plan-cn.xiaomimimo.com/anthropic"
    "XIAOMI_TOKEN_PLAN_CN_API_KEY";
  check "xiaomi-token-plan-ams"
    "https://token-plan-ams.xiaomimimo.com/anthropic"
    "XIAOMI_TOKEN_PLAN_AMS_API_KEY";
  check "xiaomi-token-plan-sgp"
    "https://token-plan-sgp.xiaomimimo.com/anthropic"
    "XIAOMI_TOKEN_PLAN_SGP_API_KEY"

let test_resolve_no_cross_region_bleed () =
  (* Only sgp set: cn/ams/public must NOT pick it up. *)
  with_xiaomi_env
    [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-only") ]
    (fun () ->
      Alcotest.(check (option string))
        "sgp set" (Some "sgp-only")
        (Xiaomi.resolve_api_key "xiaomi-token-plan-sgp");
      Alcotest.(check (option string))
        "cn unset" None
        (Xiaomi.resolve_api_key "xiaomi-token-plan-cn");
      Alcotest.(check (option string))
        "ams unset" None
        (Xiaomi.resolve_api_key "xiaomi-token-plan-ams");
      Alcotest.(check (option string))
        "public unset" None
        (Xiaomi.resolve_api_key "xiaomi"))

let test_resolve_unknown_provider () =
  with_xiaomi_env [] (fun () ->
      Alcotest.(check (option string))
        "unknown" None
        (Xiaomi.resolve_api_key "not-a-xiaomi-provider"))

(* --- ~/.mimo fallback for sgp --- *)

let test_mimo_raw_fallback () =
  Test_helpers.with_temp_home (fun home ->
      with_xiaomi_env [] (fun () ->
          write_file (Filename.concat home ".mimo") "tp-rawkey\n";
          Alcotest.(check (option string))
            "sgp falls back to ~/.mimo" (Some "tp-rawkey")
            (Xiaomi.resolve_api_key "xiaomi-token-plan-sgp");
          (* Non-sgp providers do NOT fall back to ~/.mimo. *)
          Alcotest.(check (option string))
            "cn no ~/.mimo fallback" None
            (Xiaomi.resolve_api_key "xiaomi-token-plan-cn")))

let test_mimo_env_takes_precedence () =
  Test_helpers.with_temp_home (fun home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "env-key") ]
        (fun () ->
          write_file (Filename.concat home ".mimo") "file-key";
          (* env var wins over ~/.mimo *)
          Alcotest.(check (option string))
            "env beats ~/.mimo" (Some "env-key")
            (Xiaomi.resolve_api_key "xiaomi-token-plan-sgp")))

let test_mimo_json_tolerated () =
  Test_helpers.with_temp_home (fun home ->
      with_xiaomi_env [] (fun () ->
          write_file
            (Filename.concat home ".mimo")
            {|{"api_key":"json-key","other":1}|};
          Alcotest.(check (option string))
            "json api_key" (Some "json-key") (Xiaomi.read_mimo_key ())))

let test_mimo_json_token_and_key_tolerated () =
  Test_helpers.with_temp_home (fun home ->
      with_xiaomi_env [] (fun () ->
          let path = Filename.concat home ".mimo" in
          write_file path {|{"token":"token-json-key"}|};
          Alcotest.(check (option string))
            "json token" (Some "token-json-key") (Xiaomi.read_mimo_key ());
          write_file path {|{"key":"plain-json-key"}|};
          Alcotest.(check (option string))
            "json key" (Some "plain-json-key") (Xiaomi.read_mimo_key ())))

let test_mimo_absent () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env [] (fun () ->
          Alcotest.(check (option string))
            "absent ~/.mimo" None (Xiaomi.read_mimo_key ())))

(* --- augment_providers --- *)

let provider_field name (ps : (string * Runtime_config.provider_config) list) =
  List.assoc_opt name ps

let test_augment_synthesizes_sgp () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key") ]
        (fun () ->
          let result = Xiaomi.augment_providers ~resolve_secrets:true [] in
          match provider_field "xiaomi-token-plan-sgp" result with
          | None -> Alcotest.fail "sgp provider not synthesized"
          | Some pc ->
              Alcotest.(check string) "api_key" "sgp-key" pc.api_key;
              Alcotest.(check (option string)) "kind" (Some "xiaomi") pc.kind;
              Alcotest.(check (option string))
                "base_url"
                (Some "https://token-plan-sgp.xiaomimimo.com/anthropic")
                pc.base_url;
              (* B713: Anthropic path uses native thinking blocks, not
                 reasoning_content. oai_thinking_style is not set. *)
              (* Providers without a discoverable key are not synthesized. *)
              Alcotest.(check bool)
                "cn not synthesized" true
                (provider_field "xiaomi-token-plan-cn" result = None);
              Alcotest.(check bool)
                "public not synthesized" true
                (provider_field "xiaomi" result = None)))

let test_augment_backfills_declared () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key") ]
        (fun () ->
          let declared =
            [
              ("xiaomi-token-plan-sgp", Runtime_config.default_provider_config);
            ]
          in
          let result =
            Xiaomi.augment_providers ~resolve_secrets:true declared
          in
          match provider_field "xiaomi-token-plan-sgp" result with
          | None -> Alcotest.fail "declared sgp provider disappeared"
          | Some pc ->
              Alcotest.(check string) "backfilled key" "sgp-key" pc.api_key;
              Alcotest.(check (option string))
                "backfilled base_url"
                (Some "https://token-plan-sgp.xiaomimimo.com/anthropic")
                pc.base_url;
              Alcotest.(check (option string))
                "backfilled kind" (Some "xiaomi") pc.kind))

let test_augment_preserves_explicit_key () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "env-key") ]
        (fun () ->
          let declared =
            [
              ( "xiaomi-token-plan-sgp",
                {
                  Runtime_config.default_provider_config with
                  api_key = "explicit-key";
                } );
            ]
          in
          let result =
            Xiaomi.augment_providers ~resolve_secrets:true declared
          in
          match provider_field "xiaomi-token-plan-sgp" result with
          | Some pc ->
              Alcotest.(check string)
                "explicit key preserved" "explicit-key" pc.api_key
          | None -> Alcotest.fail "provider missing"))

let test_augment_no_key_no_synth () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env [] (fun () ->
          let result = Xiaomi.augment_providers ~resolve_secrets:true [] in
          Alcotest.(check (list string))
            "no providers synthesized" [] (List.map fst result)))

let test_augment_noop_when_no_resolve () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key") ]
        (fun () ->
          let declared =
            [
              ("xiaomi-token-plan-sgp", Runtime_config.default_provider_config);
            ]
          in
          let result =
            Xiaomi.augment_providers ~resolve_secrets:false declared
          in
          (* unchanged: no synthesis and no backfill *)
          Alcotest.(check (list string))
            "names unchanged"
            [ "xiaomi-token-plan-sgp" ]
            (List.map fst result);
          match provider_field "xiaomi-token-plan-sgp" result with
          | Some pc ->
              Alcotest.(check string) "key not injected" "" pc.api_key;
              Alcotest.(check (option string))
                "base_url not set" None pc.base_url;
              Alcotest.(check (option string)) "kind not set" None pc.kind;
              Alcotest.(check string)
                "thinking unchanged" "none" pc.oai_thinking_style
          | None -> Alcotest.fail "provider disappeared"))

(* --- models_for / catalog / pricing / reasoning --- *)

let test_models_for_token_plan_omits_flash () =
  let sgp = spec_ids (Xiaomi.models_for "xiaomi-token-plan-sgp") in
  Alcotest.(check int) "token-plan has 5 models" 5 (List.length sgp);
  Alcotest.(check bool)
    "no flash on token-plan" false
    (List.mem "mimo-v2-flash" sgp);
  Alcotest.(check bool) "has v2.5-pro" true (List.mem "mimo-v2.5-pro" sgp)

let test_models_for_public_has_flash () =
  let pub = spec_ids (Xiaomi.models_for "xiaomi") in
  Alcotest.(check int) "public has 6 models" 6 (List.length pub);
  Alcotest.(check bool) "public has flash" true (List.mem "mimo-v2-flash" pub)

let test_catalog_by_provider () =
  let models = Models_catalog.by_provider "xiaomi-token-plan-sgp" in
  Alcotest.(check int) "5 catalog models" 5 (List.length models);
  Alcotest.(check bool)
    "catalog has v2.5-pro" true
    (List.exists
       (fun (m : Models_catalog.model_info) -> m.id = "mimo-v2.5-pro")
       models)

let check_price label model exp_in exp_out =
  match Cost_tracker.lookup_pricing model with
  | None -> Alcotest.failf "%s: no pricing for %s" label model
  | Some p ->
      Alcotest.(check (float 0.0001)) (label ^ " input") exp_in p.input_per_m;
      Alcotest.(check (float 0.0001)) (label ^ " output") exp_out p.output_per_m

let test_pricing_lookup () =
  check_price "sgp-qualified" "xiaomi-token-plan-sgp:mimo-v2.5-pro" 1.0 3.0;
  check_price "bare flash" "mimo-v2-flash" 0.1 0.3;
  check_price "public-qualified" "xiaomi:mimo-v2-flash" 0.1 0.3

let test_flash_cache_read () =
  match Cost_tracker.lookup_pricing "mimo-v2-flash" with
  | Some { cache_read_per_m = Some c; _ } ->
      Alcotest.(check (float 0.0001)) "flash cache read" 0.01 c
  | _ -> Alcotest.fail "flash cache_read should be Some 0.01"

(* Zero-config: even with no config.json on disk, a discoverable key surfaces
   the provider via Config_loader.load_readonly (default_with_discovered_providers). *)
let test_load_readonly_zero_config () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key") ]
        (fun () ->
          let cfg = Config_loader.load_readonly () in
          match List.assoc_opt "xiaomi-token-plan-sgp" cfg.providers with
          | Some pc ->
              Alcotest.(check string) "zero-config key" "sgp-key" pc.api_key
          | None ->
              Alcotest.fail "sgp provider not synthesized with no config file"))

let test_requires_reasoning_content () =
  Alcotest.(check bool)
    "mimo requires reasoning_content" true
    (Provider.model_requires_reasoning_content "mimo-v2.5-pro");
  Alcotest.(check bool)
    "provider-qualified mimo requires reasoning_content" false
    (Provider.model_requires_reasoning_content "xiaomi:mimo-v2.5-pro");
  Alcotest.(check bool)
    "minimax does not collide" false
    (Provider.model_requires_reasoning_content "minimax-m2.7");
  Alcotest.(check bool)
    "non-mimo does not" false
    (Provider.model_requires_reasoning_content "gpt-5.4")

let test_detect_kind_xiaomi_anthropic () =
  let pc =
    {
      Runtime_config.default_provider_config with
      api_key = "xiaomi-key";
      kind = Some "xiaomi";
    }
  in
  (* B713: xiaomi now routes through Anthropic-compatible endpoint. *)
  Alcotest.(check bool)
    "xiaomi kind routes Anthropic" true
    (Provider.detect_kind pc = Provider.Anthropic)

let test_model_specs_match_reference () =
  let public = Xiaomi.models_for "xiaomi" in
  let token_plan = Xiaomi.models_for "xiaomi-token-plan-sgp" in
  let public_ids = spec_ids public in
  let token_ids = spec_ids token_plan in
  Alcotest.(check (list string))
    "public ids"
    [
      "mimo-v2-flash";
      "mimo-v2-omni";
      "mimo-v2-pro";
      "mimo-v2.5";
      "mimo-v2.5-pro";
      "mimo-v2.5-pro-ultraspeed";
    ]
    public_ids;
  Alcotest.(check (list string))
    "token-plan ids"
    [
      "mimo-v2-omni";
      "mimo-v2-pro";
      "mimo-v2.5";
      "mimo-v2.5-pro";
      "mimo-v2.5-pro-ultraspeed";
    ]
    token_ids;
  check_spec "flash"
    (spec_by_id public "mimo-v2-flash")
    ~context_window:262144 ~max_tokens:65536 ~supports_vision:false
    ~input_per_m:0.1 ~output_per_m:0.3 ~cache_read_per_m:0.01;
  check_spec "omni"
    (spec_by_id public "mimo-v2-omni")
    ~context_window:262144 ~max_tokens:131072 ~supports_vision:true
    ~input_per_m:0.4 ~output_per_m:2.0 ~cache_read_per_m:0.08;
  check_spec "pro"
    (spec_by_id public "mimo-v2-pro")
    ~context_window:1048576 ~max_tokens:131072 ~supports_vision:false
    ~input_per_m:1.0 ~output_per_m:3.0 ~cache_read_per_m:0.2;
  check_spec "v2.5"
    (spec_by_id public "mimo-v2.5")
    ~context_window:1048576 ~max_tokens:131072 ~supports_vision:true
    ~input_per_m:0.4 ~output_per_m:2.0 ~cache_read_per_m:0.08;
  check_spec "v2.5-pro"
    (spec_by_id public "mimo-v2.5-pro")
    ~context_window:1048576 ~max_tokens:131072 ~supports_vision:false
    ~input_per_m:1.0 ~output_per_m:3.0 ~cache_read_per_m:0.2;
  check_spec "ultraspeed"
    (spec_by_id public "mimo-v2.5-pro-ultraspeed")
    ~context_window:1048576 ~max_tokens:131072 ~supports_vision:false
    ~input_per_m:1.305 ~output_per_m:2.61 ~cache_read_per_m:0.0108

let test_catalog_all_xiaomi_models () =
  let check_count provider expected =
    Alcotest.(check int)
      (provider ^ " catalog count")
      expected
      (List.length (Models_catalog.by_provider provider))
  in
  check_count "xiaomi" 6;
  check_count "xiaomi-token-plan-cn" 5;
  check_count "xiaomi-token-plan-ams" 5;
  check_count "xiaomi-token-plan-sgp" 5;
  let flash = Models_catalog.find_by_full_name "xiaomi:mimo-v2-flash" in
  let sgp_flash =
    Models_catalog.find_by_full_name "xiaomi-token-plan-sgp:mimo-v2-flash"
  in
  Alcotest.(check bool) "public flash cataloged" true (Option.is_some flash);
  Alcotest.(check bool)
    "token-plan flash omitted" true (Option.is_none sgp_flash)

let test_synthesized_provider_not_serialized_from_display_parse () =
  Test_helpers.with_temp_home (fun _home ->
      with_xiaomi_env
        [ ("XIAOMI_TOKEN_PLAN_SGP_API_KEY", "sgp-key") ]
        (fun () ->
          let cfg =
            Config_loader.parse_config ~resolve_secrets:false (`Assoc [])
          in
          let json = Runtime_config.to_json cfg in
          assert_no_provider_json "xiaomi-token-plan-sgp" json))

let suite =
  [
    Alcotest.test_case "provider defs" `Quick test_provider_defs;
    Alcotest.test_case "resolve env per region" `Quick
      test_resolve_env_per_region;
    Alcotest.test_case "resolve no cross-region bleed" `Quick
      test_resolve_no_cross_region_bleed;
    Alcotest.test_case "resolve unknown provider" `Quick
      test_resolve_unknown_provider;
    Alcotest.test_case "~/.mimo raw fallback (sgp only)" `Quick
      test_mimo_raw_fallback;
    Alcotest.test_case "env beats ~/.mimo" `Quick test_mimo_env_takes_precedence;
    Alcotest.test_case "~/.mimo JSON tolerated" `Quick test_mimo_json_tolerated;
    Alcotest.test_case "~/.mimo JSON token/key tolerated" `Quick
      test_mimo_json_token_and_key_tolerated;
    Alcotest.test_case "~/.mimo absent" `Quick test_mimo_absent;
    Alcotest.test_case "augment synthesizes sgp" `Quick
      test_augment_synthesizes_sgp;
    Alcotest.test_case "augment backfills declared" `Quick
      test_augment_backfills_declared;
    Alcotest.test_case "augment preserves explicit key" `Quick
      test_augment_preserves_explicit_key;
    Alcotest.test_case "augment no key no synth" `Quick
      test_augment_no_key_no_synth;
    Alcotest.test_case "augment no-op when resolve_secrets=false" `Quick
      test_augment_noop_when_no_resolve;
    Alcotest.test_case "models_for token-plan omits flash" `Quick
      test_models_for_token_plan_omits_flash;
    Alcotest.test_case "models_for public has flash" `Quick
      test_models_for_public_has_flash;
    Alcotest.test_case "catalog by_provider sgp" `Quick test_catalog_by_provider;
    Alcotest.test_case "pricing lookup" `Quick test_pricing_lookup;
    Alcotest.test_case "flash cache read" `Quick test_flash_cache_read;
    Alcotest.test_case "load_readonly zero-config synth" `Quick
      test_load_readonly_zero_config;
    Alcotest.test_case "requires reasoning_content" `Quick
      test_requires_reasoning_content;
    Alcotest.test_case "detect kind xiaomi is Anthropic" `Quick
      test_detect_kind_xiaomi_anthropic;
    Alcotest.test_case "model specs match reference" `Quick
      test_model_specs_match_reference;
    Alcotest.test_case "catalog all xiaomi models" `Quick
      test_catalog_all_xiaomi_models;
    Alcotest.test_case "display parse does not serialize synth provider" `Quick
      test_synthesized_provider_not_serialized_from_display_parse;
  ]
