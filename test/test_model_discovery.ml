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

let suite =
  [
    ("load codex file models", `Quick, test_load_codex_file_models);
    ("load codex missing file", `Quick, test_load_codex_missing_file);
  ]
