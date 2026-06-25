let test_load_codex_file_models () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      Memory_0_schema.init_model_discovery_state_schema db;
      let dir = Filename.temp_file "codex_test_" "" in
      Unix.unlink dir;
      Unix.mkdir dir 0o700;
      let file_path = Filename.concat dir "models_cache.json" in
      let json_content =
        {|{
  "fetched_at": "2026-03-19T00:00:00Z",
  "models": [
    {
      "slug": "gpt-5.4-test",
      "display_name": "GPT 5.4 Test",
      "context_window": 272000,
      "input_modalities": ["text", "image"],
      "supports_parallel_tool_calls": true,
      "supported_reasoning_levels": [{"effort": "medium"}]
    },
    {
      "slug": "gpt-5-codex-mini-test",
      "display_name": "GPT 5 Codex Mini Test",
      "context_window": 128000,
      "input_modalities": ["text"],
      "supports_parallel_tool_calls": false,
      "supported_reasoning_levels": []
    }
  ]
}|}
      in
      let oc = open_out file_path in
      output_string oc json_content;
      close_out oc;
      let count =
        Model_discovery.load_codex_file_models ~path:(Some file_path) ~db ()
      in
      Alcotest.(check int) "loaded 2 models" 2 count;
      let rows = ref [] in
      let stmt =
        Sqlite3.prepare db
          "SELECT provider, model_id, display_name, context_window, \
           supports_vision, supports_tools, supports_thinking, source FROM \
           models_cache ORDER BY model_id"
      in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let provider =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let model_id =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let display_name =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let context_window =
          match Sqlite3.column stmt 3 with
          | Sqlite3.Data.INT n -> Some (Int64.to_int n)
          | _ -> None
        in
        let supports_vision =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.INT n -> n = 1L
          | _ -> false
        in
        let supports_tools =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.INT n -> n = 1L
          | _ -> false
        in
        let supports_thinking =
          match Sqlite3.column stmt 6 with
          | Sqlite3.Data.INT n -> n = 1L
          | _ -> false
        in
        let source =
          match Sqlite3.column stmt 7 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        rows :=
          ( provider,
            model_id,
            display_name,
            context_window,
            supports_vision,
            supports_tools,
            supports_thinking,
            source )
          :: !rows
      done;
      ignore (Sqlite3.finalize stmt);
      let rows = List.rev !rows in
      Alcotest.(check int) "2 rows in DB" 2 (List.length rows);
      let mini =
        List.find
          (fun (_, mid, _, _, _, _, _, _) -> mid = "gpt-5-codex-mini-test")
          rows
      in
      let p, _, dn, cw, sv, st, sth, src = mini in
      Alcotest.(check string) "provider" "openai-codex" p;
      Alcotest.(check (option string))
        "display_name" (Some "GPT 5 Codex Mini Test") dn;
      Alcotest.(check (option int)) "context_window" (Some 128000) cw;
      Alcotest.(check bool) "no vision" false sv;
      Alcotest.(check bool) "no parallel tools" false st;
      Alcotest.(check bool) "no thinking" false sth;
      Alcotest.(check string) "source" "codex-cli" src;
      let full =
        List.find (fun (_, mid, _, _, _, _, _, _) -> mid = "gpt-5.4-test") rows
      in
      let p2, _, _, cw2, sv2, st2, sth2, _ = full in
      Alcotest.(check string) "provider" "openai-codex" p2;
      Alcotest.(check (option int)) "context_window" (Some 272000) cw2;
      Alcotest.(check bool) "has vision" true sv2;
      Alcotest.(check bool) "has tools" true st2;
      Alcotest.(check bool) "has thinking" true sth2;
      Unix.unlink file_path;
      Unix.rmdir dir)

let test_load_codex_missing_file () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      let count =
        Model_discovery.load_codex_file_models
          ~path:(Some "/nonexistent/models_cache.json") ~db ()
      in
      Alcotest.(check int) "0 when missing" 0 count)

let query_one_string db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> "")
      | _ -> "")

let query_one_int db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Alcotest.failf "sqlite exec failed (%s): %s" (Sqlite3.Rc.to_string rc) sql

let test_seed_catalog_models_populates_models_cache () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      let seeded = Model_discovery.seed_catalog_models ~db in
      Alcotest.(check bool) "seeded some catalog rows" true (seeded > 0);
      Alcotest.(check string)
        "source" "catalog"
        (query_one_string db
           "SELECT source FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-5.4'");
      Alcotest.(check int)
        "context" 1050000
        (query_one_int db
           "SELECT context_window FROM models_cache WHERE provider = 'openai' \
            AND model_id = 'gpt-5.4'");
      Alcotest.(check int)
        "deprecated false" 0
        (query_one_int db
           "SELECT deprecated FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-5.4'");
      Alcotest.(check int)
        "unavailable false" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-5.4'"))

let test_seed_catalog_models_is_idempotent () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      ignore (Model_discovery.seed_catalog_models ~db);
      let before = query_one_int db "SELECT COUNT(*) FROM models_cache" in
      ignore (Model_discovery.seed_catalog_models ~db);
      let after = query_one_int db "SELECT COUNT(*) FROM models_cache" in
      Alcotest.(check int) "row count stable" before after)

let test_seed_catalog_models_preserves_codex_cli_metadata () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      Alcotest.(check bool)
        "inserted" true
        (Model_discovery.upsert_model_rich ~db ~provider:"openai-codex"
           ~model_id:"gpt-5.3-codex" ~display_name:(Some "From Codex")
           ~context_window:(Some 999) ~supports_vision:false
           ~supports_tools:false ~supports_thinking:false ~source:"codex-cli" ());
      ignore (Model_discovery.seed_catalog_models ~db);
      Alcotest.(check string)
        "source preserved" "codex-cli"
        (query_one_string db
           "SELECT source FROM models_cache WHERE provider = 'openai-codex' \
            AND model_id = 'gpt-5.3-codex'");
      Alcotest.(check string)
        "display preserved" "From Codex"
        (query_one_string db
           "SELECT display_name FROM models_cache WHERE provider = \
            'openai-codex' AND model_id = 'gpt-5.3-codex'"))

let test_seed_catalog_models_updates_catalog_status_flags () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      exec db
        "INSERT INTO models_cache (provider, model_id, display_name, \
         context_window, supports_vision, supports_tools, supports_thinking, \
         source, deprecated, unavailable) VALUES ('openai', 'gpt-4', 'From \
         Provider', 999, 1, 1, 1, 'provider-api', 0, 0)";
      ignore (Model_discovery.seed_catalog_models ~db);
      Alcotest.(check string)
        "source preserved" "provider-api"
        (query_one_string db
           "SELECT source FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-4'");
      Alcotest.(check string)
        "display preserved" "From Provider"
        (query_one_string db
           "SELECT display_name FROM models_cache WHERE provider = 'openai' \
            AND model_id = 'gpt-4'");
      Alcotest.(check int)
        "context preserved" 999
        (query_one_int db
           "SELECT context_window FROM models_cache WHERE provider = 'openai' \
            AND model_id = 'gpt-4'");
      Alcotest.(check int)
        "catalog deprecated authoritative" 1
        (query_one_int db
           "SELECT deprecated FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-4'");
      Alcotest.(check int)
        "catalog unavailable authoritative" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openai' AND \
            model_id = 'gpt-4'"))

let test_provider_refresh_preserves_catalog_metadata () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      ignore (Model_discovery.seed_catalog_models ~db);
      let count =
        Model_discovery.upsert_models ~db ~provider:"openai" [ "gpt-5.4" ]
      in
      Alcotest.(check int) "upserted one id" 1 count;
      Alcotest.(check int)
        "context preserved" 1050000
        (query_one_int db
           "SELECT context_window FROM models_cache WHERE provider = 'openai' \
            AND model_id = 'gpt-5.4'");
      Alcotest.(check int)
        "supports thinking preserved" 1
        (query_one_int db
           "SELECT supports_thinking FROM models_cache WHERE provider = \
            'openai' AND model_id = 'gpt-5.4'"))

let test_provider_refresh_reconciles_provider_api_availability () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      exec db
        "INSERT INTO models_cache (provider, model_id, display_name, \
         context_window, supports_vision, supports_tools, supports_thinking, \
         source, deprecated, unavailable) VALUES ('openrouter', \
         'returned-model', 'Rich Returned', 12345, 1, 1, 1, 'provider-api', 0, \
         1)";
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'stale-model', 'provider-api', 0, \
         0)";
      let count =
        Model_discovery.upsert_models ~db ~provider:"openrouter"
          [ "returned-model"; "new-model" ]
      in
      Alcotest.(check int) "upserted returned rows" 2 count;
      Alcotest.(check int)
        "returned row made available" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openrouter' \
            AND model_id = 'returned-model'");
      Alcotest.(check string)
        "display metadata preserved" "Rich Returned"
        (query_one_string db
           "SELECT display_name FROM models_cache WHERE provider = \
            'openrouter' AND model_id = 'returned-model'");
      Alcotest.(check int)
        "context metadata preserved" 12345
        (query_one_int db
           "SELECT context_window FROM models_cache WHERE provider = \
            'openrouter' AND model_id = 'returned-model'");
      Alcotest.(check int)
        "stale provider-api row retired" 1
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openrouter' \
            AND model_id = 'stale-model'");
      Alcotest.(check int)
        "new row available" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openrouter' \
            AND model_id = 'new-model'"))

let test_provider_refresh_does_not_retire_non_provider_api_rows () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'catalog-only', 'catalog', 0, 0)";
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'codex-only', 'codex-cli', 0, 0)";
      ignore
        (Model_discovery.upsert_models ~db ~provider:"openrouter"
           [ "returned-model" ]);
      Alcotest.(check int)
        "catalog row remains available" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openrouter' \
            AND model_id = 'catalog-only'");
      Alcotest.(check int)
        "codex-cli row remains available" 0
        (query_one_int db
           "SELECT unavailable FROM models_cache WHERE provider = 'openrouter' \
            AND model_id = 'codex-only'"))

let test_xiaomi_kind_is_refreshable_with_key_and_base_url () =
  let pc =
    {
      Runtime_config.default_provider_config with
      api_key = "sk-xiaomi-test";
      kind = Some "xiaomi";
      base_url = Some "http://127.0.0.1:1/v1";
    }
  in
  Alcotest.(check bool)
    "xiaomi with key is not skipped" false
    (Model_discovery.should_skip_provider ~name:"xiaomi" pc)

let test_get_db_only_model_infos_excludes_catalog_and_marks_flags () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      ignore (Model_discovery.seed_catalog_models ~db);
      exec db
        "INSERT INTO models_cache (provider, model_id, display_name, \
         context_window, supports_vision, supports_tools, supports_thinking, \
         source, deprecated, unavailable) VALUES ('openrouter', 'custom-live', \
         'Custom Live', 12345, 1, 1, 0, 'provider-api', 0, 0)";
      let infos =
        Model_discovery.get_db_only_model_infos ~db
          ~provider_filter:(Some "openrouter") ()
      in
      Alcotest.(check int) "one db-only info" 1 (List.length infos);
      let info = List.hd infos in
      Alcotest.(check string)
        "provider" "openrouter" info.Models_catalog.provider;
      Alcotest.(check string) "model id" "custom-live" info.Models_catalog.id;
      Alcotest.(check (option int))
        "context" (Some 12345) info.Models_catalog.context_window;
      Alcotest.(check bool) "vision" true info.Models_catalog.supports_vision;
      Alcotest.(check bool) "deprecated" false info.Models_catalog.deprecated)

let test_cached_model_disallowed_when_deprecated_or_unavailable () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'old-model', 'provider-api', 1, 0)";
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'off-model', 'provider-api', 0, 1)";
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'live-model', 'provider-api', 0, \
         0)";
      Alcotest.(check bool)
        "deprecated disallowed" true
        (Model_discovery.cached_model_disallowed ~db "openrouter:old-model");
      Alcotest.(check bool)
        "unavailable disallowed" true
        (Model_discovery.cached_model_disallowed ~db "openrouter:off-model");
      Alcotest.(check bool)
        "live not disallowed" false
        (Model_discovery.cached_model_disallowed ~db "openrouter:live-model");
      Alcotest.(check bool)
        "live exists" true
        (Model_discovery.cached_model_exists ~db "openrouter:live-model"))

let test_cached_model_status_resolves_known_plain_catalog_name () =
  Test_helpers.with_memory_db (fun db ->
      Memory_0_schema.init_models_cache_schema db;
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openai', 'gpt-4', 'catalog', 1, 0)";
      exec db
        "INSERT INTO models_cache (provider, model_id, source, deprecated, \
         unavailable) VALUES ('openrouter', 'old-model', 'provider-api', 1, 0)";
      Alcotest.(check bool)
        "known plain catalog model is disallowed" true
        (Model_discovery.cached_model_disallowed ~db "gpt-4");
      Alcotest.(check bool)
        "unknown plain db-only model is not invented" false
        (Model_discovery.cached_model_disallowed ~db "old-model"))

(* B676: provider configs without an explicit `kind` should still be skipped
   when the provider NAME matches a known-non-/v1/models-supporting backend
   (e.g. kimi_coding, zai_coding). Previously these triggered HTTP 401. *)
let make_pc api_key kind : Runtime_config.provider_config =
  { Runtime_config.default_provider_config with api_key; kind }

let test_skip_by_name_when_kind_absent () =
  let pc = make_pc "sk-kimi-fake-key-for-test" None in
  (* with no kind, but provider name is in skip list, should still skip *)
  Alcotest.(check bool)
    "kimi_coding skipped by name" true
    (Model_discovery.should_skip_provider ~name:"kimi_coding" pc);
  Alcotest.(check bool)
    "zai_coding skipped by name" true
    (Model_discovery.should_skip_provider ~name:"zai_coding" pc);
  Alcotest.(check bool)
    "unknown name not skipped" false
    (Model_discovery.should_skip_provider ~name:"some_unknown_provider" pc);
  (* with explicit kind, the kind wins *)
  let pc_with_kind = make_pc "sk-x" (Some "anthropic") in
  Alcotest.(check bool)
    "explicit kind=anthropic skipped" true
    (Model_discovery.should_skip_provider ~name:"kimi_coding" pc_with_kind);
  (* empty api_key skipped regardless of name *)
  let pc_empty = make_pc "" None in
  Alcotest.(check bool)
    "empty api_key skipped" true
    (Model_discovery.should_skip_provider ~name:"openrouter" pc_empty)

let suite =
  [
    ("load codex file models", `Quick, test_load_codex_file_models);
    ("load codex missing file", `Quick, test_load_codex_missing_file);
    ( "seed_catalog_models populates models_cache",
      `Quick,
      test_seed_catalog_models_populates_models_cache );
    ( "seed_catalog_models is idempotent",
      `Quick,
      test_seed_catalog_models_is_idempotent );
    ( "seed_catalog_models preserves codex-cli metadata",
      `Quick,
      test_seed_catalog_models_preserves_codex_cli_metadata );
    ( "seed_catalog_models updates catalog status flags",
      `Quick,
      test_seed_catalog_models_updates_catalog_status_flags );
    ( "provider refresh preserves catalog metadata",
      `Quick,
      test_provider_refresh_preserves_catalog_metadata );
    ( "provider refresh reconciles provider-api availability",
      `Quick,
      test_provider_refresh_reconciles_provider_api_availability );
    ( "provider refresh does not retire non-provider-api rows",
      `Quick,
      test_provider_refresh_does_not_retire_non_provider_api_rows );
    ( "xiaomi kind is refreshable with key/base_url",
      `Quick,
      test_xiaomi_kind_is_refreshable_with_key_and_base_url );
    ( "get_db_only_model_infos excludes catalog and marks flags",
      `Quick,
      test_get_db_only_model_infos_excludes_catalog_and_marks_flags );
    ( "cached model disallowed when deprecated or unavailable",
      `Quick,
      test_cached_model_disallowed_when_deprecated_or_unavailable );
    ( "cached model status resolves known plain catalog name",
      `Quick,
      test_cached_model_status_resolves_known_plain_catalog_name );
    ( "skip by name when kind absent (B676)",
      `Quick,
      test_skip_by_name_when_kind_absent );
  ]
