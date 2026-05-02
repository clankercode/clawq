let tmp_db () =
  let path = Filename.temp_file "test_summarizer" ".db" in
  let db = Sqlite3.db_open path in
  ignore (Sqlite3.exec db "PRAGMA busy_timeout = 5000");
  Summary_store.init_schema db;
  (db, path)

let cleanup_db (db, path) =
  ignore (Sqlite3.db_close db);
  try Unix.unlink path with _ -> ()

(* -- Pmodel tests -- *)

let test_pmodel_parse_valid () =
  match Pmodel.parse "groq:openai/gpt-oss-120b" with
  | Ok t ->
      Alcotest.(check string) "provider" "groq" (Pmodel.provider t);
      Alcotest.(check string) "model" "openai/gpt-oss-120b" (Pmodel.model t);
      Alcotest.(check string)
        "to_string" "groq:openai/gpt-oss-120b" (Pmodel.to_string t)
  | Error e -> Alcotest.fail ("unexpected parse error: " ^ e)

let test_pmodel_parse_simple () =
  match Pmodel.parse "openai:gpt-4" with
  | Ok t ->
      Alcotest.(check string) "provider" "openai" (Pmodel.provider t);
      Alcotest.(check string) "model" "gpt-4" (Pmodel.model t)
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

let test_pmodel_parse_no_colon () =
  match Pmodel.parse "no-colon-here" with
  | Ok _ -> Alcotest.fail "expected error for missing colon"
  | Error _ -> ()

let test_pmodel_parse_empty_provider () =
  match Pmodel.parse ":model" with
  | Ok _ -> Alcotest.fail "expected error for empty provider"
  | Error _ -> ()

let test_pmodel_parse_empty_model () =
  match Pmodel.parse "provider:" with
  | Ok _ -> Alcotest.fail "expected error for empty model"
  | Error _ -> ()

let test_pmodel_parse_exn () =
  let t = Pmodel.parse_exn "foo:bar" in
  Alcotest.(check string) "provider" "foo" (Pmodel.provider t);
  Alcotest.(check string) "model" "bar" (Pmodel.model t)

let test_pmodel_parse_exn_invalid () =
  try
    ignore (Pmodel.parse_exn "no-colon");
    Alcotest.fail "expected Invalid_argument"
  with Invalid_argument _ -> ()

(* -- Pmodel.parse_flexible tests -- *)

let test_pmodel_flexible_canonical () =
  let f = Pmodel.parse_flexible "openai:gpt-5.4" in
  Alcotest.(check (option string))
    "provider" (Some "openai") f.Pmodel.f_provider;
  Alcotest.(check string) "model" "gpt-5.4" f.f_model;
  Alcotest.(check string) "raw" "openai:gpt-5.4" f.f_raw;
  (match f.f_format with
  | Pmodel.Canonical -> ()
  | _ -> Alcotest.fail "expected Canonical format");
  Alcotest.(check (option string))
    "no deprecation warning" None
    (Pmodel.deprecation_warning f)

let test_pmodel_flexible_legacy () =
  let f = Pmodel.parse_flexible "openai/gpt-5.4" in
  Alcotest.(check (option string))
    "provider" (Some "openai") f.Pmodel.f_provider;
  Alcotest.(check string) "model" "gpt-5.4" f.f_model;
  (match f.f_format with
  | Pmodel.Legacy -> ()
  | _ -> Alcotest.fail "expected Legacy format");
  match Pmodel.deprecation_warning f with
  | Some w ->
      Alcotest.(check bool)
        "contains deprecated" true
        (String.length w > 0
        &&
          try
            ignore (Str.search_forward (Str.regexp_string "deprecated") w 0);
            true
          with Not_found -> false)
  | None -> Alcotest.fail "expected deprecation warning for legacy format"

let test_pmodel_flexible_bare () =
  let f = Pmodel.parse_flexible "gpt-5.4" in
  Alcotest.(check (option string)) "provider" None f.Pmodel.f_provider;
  Alcotest.(check string) "model" "gpt-5.4" f.f_model;
  (match f.f_format with
  | Pmodel.Bare -> ()
  | _ -> Alcotest.fail "expected Bare format");
  match Pmodel.deprecation_warning f with
  | Some w ->
      Alcotest.(check bool) "contains no provider" true (String.length w > 0)
  | None -> Alcotest.fail "expected deprecation warning for bare format"

let test_pmodel_flexible_trims () =
  let f = Pmodel.parse_flexible "  openai:gpt-5.4  " in
  Alcotest.(check string) "raw" "openai:gpt-5.4" f.Pmodel.f_raw;
  match f.f_format with
  | Pmodel.Canonical -> ()
  | _ -> Alcotest.fail "expected Canonical format after trim"

let test_pmodel_flexible_to_canonical () =
  let legacy = Pmodel.parse_flexible "openai/gpt-5.4" in
  (match Pmodel.flexible_to_canonical legacy ~default_provider:None with
  | Some t ->
      Alcotest.(check string)
        "canonical raw" "openai:gpt-5.4" (Pmodel.to_string t);
      Alcotest.(check string) "provider" "openai" (Pmodel.provider t);
      Alcotest.(check string) "model" "gpt-5.4" (Pmodel.model t)
  | None -> Alcotest.fail "expected Some for legacy format");
  let bare = Pmodel.parse_flexible "gpt-5.4" in
  (match Pmodel.flexible_to_canonical bare ~default_provider:None with
  | Some _ -> Alcotest.fail "expected None for bare with no default_provider"
  | None -> ());
  match Pmodel.flexible_to_canonical bare ~default_provider:(Some "openai") with
  | Some t ->
      Alcotest.(check string)
        "canonical raw" "openai:gpt-5.4" (Pmodel.to_string t)
  | None -> Alcotest.fail "expected Some for bare with default_provider"

let test_pmodel_deprecation_warning_primary_model () =
  let ad_canonical =
    {
      Runtime_config.default.agent_defaults with
      primary_model = "openai:gpt-5.4";
    }
  in
  Alcotest.(check (option string))
    "no warning for canonical" None
    (Runtime_config.primary_model_deprecation_warning ad_canonical);
  let ad_legacy =
    {
      Runtime_config.default.agent_defaults with
      primary_model = "openai/gpt-5.4";
    }
  in
  (match Runtime_config.primary_model_deprecation_warning ad_legacy with
  | Some _ -> ()
  | None -> Alcotest.fail "expected warning for legacy format");
  let ad_bare =
    { Runtime_config.default.agent_defaults with primary_model = "gpt-5.4" }
  in
  match Runtime_config.primary_model_deprecation_warning ad_bare with
  | Some _ -> ()
  | None -> Alcotest.fail "expected warning for bare format"

(* -- Summary_store tests -- *)

let make_record ?(summary_id = "sum_test123456") ?(session_key = "test_session")
    ?(tool_name = "file_read") ?(original = "original content here")
    ?(summary = "summarized") ?(context = "") ?(model = "groq:test") () :
    Summary_store.summary_record =
  {
    summary_id;
    session_key;
    tool_name;
    original_content = original;
    summary_content = summary;
    context_snippet = context;
    original_bytes = String.length original;
    original_lines = Summary_store.count_lines original;
    original_tokens_est = Summary_store.estimate_tokens original;
    summary_bytes = String.length summary;
    summary_lines = Summary_store.count_lines summary;
    summary_tokens_est = Summary_store.estimate_tokens summary;
    model_used = model;
    created_at = "2026-03-11T00:00:00Z";
  }

let test_store_and_find () =
  let handle = tmp_db () in
  let db = fst handle in
  let record = make_record () in
  Summary_store.store ~db record;
  (match Summary_store.find ~db ~summary_id:"sum_test123456" with
  | None -> Alcotest.fail "expected to find stored record"
  | Some r ->
      Alcotest.(check string) "summary_id" "sum_test123456" r.summary_id;
      Alcotest.(check string) "session_key" "test_session" r.session_key;
      Alcotest.(check string) "tool_name" "file_read" r.tool_name;
      Alcotest.(check string)
        "original_content" "original content here" r.original_content;
      Alcotest.(check string) "summary_content" "summarized" r.summary_content;
      Alcotest.(check string) "model_used" "groq:test" r.model_used);
  cleanup_db handle

let test_find_not_found () =
  let handle = tmp_db () in
  let db = fst handle in
  (match Summary_store.find ~db ~summary_id:"sum_nonexistent" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for nonexistent ID");
  cleanup_db handle

let test_delete_for_session () =
  let handle = tmp_db () in
  let db = fst handle in
  Summary_store.store ~db (make_record ~session_key:"s1" ~summary_id:"sum_a" ());
  Summary_store.store ~db (make_record ~session_key:"s1" ~summary_id:"sum_b" ());
  Summary_store.store ~db (make_record ~session_key:"s2" ~summary_id:"sum_c" ());
  Summary_store.delete_for_session ~db ~session_key:"s1";
  Alcotest.(check bool)
    "s1 row a deleted" true
    (Summary_store.find ~db ~summary_id:"sum_a" = None);
  Alcotest.(check bool)
    "s1 row b deleted" true
    (Summary_store.find ~db ~summary_id:"sum_b" = None);
  Alcotest.(check bool)
    "s2 row c kept" true
    (Summary_store.find ~db ~summary_id:"sum_c" <> None);
  cleanup_db handle

let test_purge_ttl () =
  let handle = tmp_db () in
  let db = fst handle in
  (* Insert a row with an old timestamp *)
  Summary_store.store ~db
    (make_record ~summary_id:"sum_old" ~original:"old data" ());
  (* Force the created_at to be old *)
  ignore
    (Sqlite3.exec db
       "UPDATE summaries SET created_at = datetime('now', '-60 days') WHERE \
        summary_id = 'sum_old'");
  Summary_store.store ~db
    (make_record ~summary_id:"sum_new" ~original:"new data" ());
  ignore
    (Sqlite3.exec db
       "UPDATE summaries SET created_at = datetime('now') WHERE summary_id = \
        'sum_new'");
  let purged = Summary_store.purge_older_than ~db ~max_age_days:30 in
  Alcotest.(check bool) "purged at least 1" true (purged >= 1);
  Alcotest.(check bool)
    "old record gone" true
    (Summary_store.find ~db ~summary_id:"sum_old" = None);
  Alcotest.(check bool)
    "new record kept" true
    (Summary_store.find ~db ~summary_id:"sum_new" <> None);
  cleanup_db handle

let test_generate_id_format () =
  Mirage_crypto_rng_unix.use_default ();
  let id = Summary_store.generate_id () in
  Alcotest.(check bool)
    "starts with sum_" true
    (String.length id > 4 && String.sub id 0 4 = "sum_");
  Alcotest.(check int) "length is 16 (sum_ + 12 hex)" 16 (String.length id)

(* -- Agent_prompt_loader tests -- *)

let test_agent_prompt_loader_missing_file () =
  let result =
    Agent_prompt_loader.load ~workspace:"/nonexistent/path"
      ~agent_name:"summarizer" ~default:"default prompt"
  in
  Alcotest.(check string) "default prompt" "default prompt" result.system_prompt;
  Alcotest.(check int) "no metadata" 0 (List.length result.metadata)

let test_agent_prompt_loader_with_frontmatter () =
  let dir = Filename.temp_dir "test_prompt" "" in
  let agents_dir = Filename.concat dir "agents" in
  Unix.mkdir agents_dir 0o755;
  let path = Filename.concat agents_dir "test.md" in
  let oc = open_out path in
  output_string oc
    "---\nversion: 1\ndescription: test prompt\n---\nThe prompt body.";
  close_out oc;
  let result =
    Agent_prompt_loader.load ~workspace:dir ~agent_name:"test"
      ~default:"fallback"
  in
  Alcotest.(check string) "prompt body" "The prompt body." result.system_prompt;
  Alcotest.(check int) "2 metadata entries" 2 (List.length result.metadata);
  Alcotest.(check string) "version" "1" (List.assoc "version" result.metadata);
  (try Unix.unlink path with _ -> ());
  (try Unix.rmdir agents_dir with _ -> ());
  try Unix.rmdir dir with _ -> ()

(* -- Summarizer passthrough tests -- *)

let test_passthrough_below_threshold () =
  let config = Runtime_config.default in
  let result =
    Lwt_main.run
      (Summarizer.maybe_summarize ~config ~db:None ~session_key:None
         ~tool_name:"test" ~history:[] ~original:"short content" ())
  in
  match result with
  | Summarizer.Passthrough s ->
      Alcotest.(check string) "unchanged" "short content" s
  | _ -> Alcotest.fail "expected Passthrough"

let test_passthrough_excluded_tool () =
  let config =
    {
      Runtime_config.default with
      summarizer =
        {
          Runtime_config.default_summarizer_config with
          excluded_tools = [ "shell_exec" ];
        };
    }
  in
  let long_content = String.make 5000 'x' in
  let result =
    Lwt_main.run
      (Summarizer.maybe_summarize ~config ~db:None ~session_key:None
         ~tool_name:"shell_exec" ~history:[] ~original:long_content ())
  in
  match result with
  | Summarizer.Passthrough s ->
      Alcotest.(check string) "unchanged" long_content s
  | _ -> Alcotest.fail "expected Passthrough for excluded tool"

let test_passthrough_disabled () =
  let config =
    {
      Runtime_config.default with
      summarizer =
        { Runtime_config.default_summarizer_config with enabled = false };
    }
  in
  let long_content = String.make 5000 'x' in
  let result =
    Lwt_main.run
      (Summarizer.maybe_summarize ~config ~db:None ~session_key:None
         ~tool_name:"test" ~history:[] ~original:long_content ())
  in
  match result with
  | Summarizer.Passthrough _ -> ()
  | _ -> Alcotest.fail "expected Passthrough when disabled"

let test_fallback_no_db () =
  let config = Runtime_config.default in
  let long_content = String.make 5000 'x' in
  let result =
    Lwt_main.run
      (Summarizer.maybe_summarize ~config ~db:None
         ~session_key:(Some "test_session") ~tool_name:"test" ~history:[]
         ~original:long_content ())
  in
  match result with
  | Summarizer.Fallback_truncated _ -> ()
  | _ -> Alcotest.fail "expected Fallback_truncated when no DB"

let test_fallback_no_session () =
  let handle = tmp_db () in
  let db = fst handle in
  let config = Runtime_config.default in
  let long_content = String.make 5000 'x' in
  let result =
    Lwt_main.run
      (Summarizer.maybe_summarize ~config ~db:(Some db) ~session_key:None
         ~tool_name:"test" ~history:[] ~original:long_content ())
  in
  (match result with
  | Summarizer.Fallback_truncated _ -> ()
  | _ -> Alcotest.fail "expected Fallback_truncated when no session");
  cleanup_db handle

(* -- Unsummarize tool tests -- *)

let test_unsummarize_basic () =
  let handle = tmp_db () in
  let db = fst handle in
  let lines = List.init 50 (fun i -> Printf.sprintf "line %d content" i) in
  let original = String.concat "\n" lines in
  Summary_store.store ~db
    (make_record ~summary_id:"sum_unsumtest01" ~original ~summary:"short" ());
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke (`Assoc [ ("summary_id", `String "sum_unsumtest01") ]))
  in
  Alcotest.(check bool)
    "contains header" true
    (String.length result > 0
    && String.sub result 0 (min 10 (String.length result)) = "[Original ");
  Alcotest.(check bool)
    "contains line 0" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "line 0 content" then has := true);
     !has);
  cleanup_db handle

let test_unsummarize_offset () =
  let handle = tmp_db () in
  let db = fst handle in
  let lines = List.init 200 (fun i -> Printf.sprintf "line-%03d" i) in
  let original = String.concat "\n" lines in
  Summary_store.store ~db
    (make_record ~summary_id:"sum_offset_test" ~original ~summary:"short" ());
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("summary_id", `String "sum_offset_test");
              ("offset", `Int 50);
              ("lines", `Int 20);
            ]))
  in
  Alcotest.(check bool)
    "contains line-050" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "line-050" then has := true);
     !has);
  Alcotest.(check bool)
    "does not contain line-000" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "line-000" then has := true);
     not !has);
  cleanup_db handle

let test_unsummarize_head_and_tail () =
  let handle = tmp_db () in
  let db = fst handle in
  let lines = List.init 500 (fun i -> Printf.sprintf "line-%03d" i) in
  let original = String.concat "\n" lines in
  Summary_store.store ~db
    (make_record ~summary_id:"sum_hat_test00" ~original ~summary:"short" ());
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("summary_id", `String "sum_hat_test00");
              ("head_and_tail", `Bool true);
              ("lines", `Int 10);
            ]))
  in
  Alcotest.(check bool)
    "contains line-000" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "line-000" then has := true);
     !has);
  Alcotest.(check bool)
    "contains line-499" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "line-499" then has := true);
     !has);
  Alcotest.(check bool)
    "contains skip marker" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l ->
         if
           String.length l > 3
           && String.sub l 0 3 = "---"
           &&
             try
               ignore (Str.search_forward (Str.regexp "skipped") l 0);
               true
             with Not_found -> false
         then has := true);
     !has);
  cleanup_db handle

let test_unsummarize_head_and_tail_overlap () =
  let handle = tmp_db () in
  let db = fst handle in
  let lines = List.init 15 (fun i -> Printf.sprintf "line-%02d" i) in
  let original = String.concat "\n" lines in
  Summary_store.store ~db
    (make_record ~summary_id:"sum_hat_overlap" ~original ~summary:"short" ());
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("summary_id", `String "sum_hat_overlap");
              ("head_and_tail", `Bool true);
              ("lines", `Int 10);
            ]))
  in
  (* 15 lines <= 10*2 = 20, so should return all content without skip marker *)
  Alcotest.(check bool)
    "no skip marker" true
    (not
       (let has = ref false in
        String.split_on_char '\n' result
        |> List.iter (fun l ->
            if
              String.length l > 3
              && String.sub l 0 3 = "---"
              &&
                try
                  ignore (Str.search_forward (Str.regexp "skipped") l 0);
                  true
                with Not_found -> false
            then has := true);
        !has));
  cleanup_db handle

let test_unsummarize_with_context () =
  let handle = tmp_db () in
  let db = fst handle in
  Summary_store.store ~db
    (make_record ~summary_id:"sum_ctx_test00" ~original:"the original"
       ~context:"[user]: hello" ~summary:"short" ());
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("summary_id", `String "sum_ctx_test00");
              ("with_context", `Bool true);
            ]))
  in
  Alcotest.(check bool)
    "contains context" true
    (let has = ref false in
     String.split_on_char '\n' result
     |> List.iter (fun l -> if l = "[user]: hello" then has := true);
     !has);
  cleanup_db handle

let test_unsummarize_not_found () =
  let handle = tmp_db () in
  let db = fst handle in
  let tool = Tools_builtin_util.unsummarize ~db in
  let result =
    Lwt_main.run
      (tool.invoke (`Assoc [ ("summary_id", `String "sum_nonexistent") ]))
  in
  Alcotest.(check bool)
    "error message" true
    (String.length result > 5 && String.sub result 0 5 = "Error");
  cleanup_db handle

(* -- Envelope format tests -- *)

let test_envelope_default () =
  let envelope =
    Summarizer.build_envelope ~summary_id:"sum_test_env000"
      ~tool_name:"shell_exec" ~model:"groq:test" ~orig_lines:100
      ~orig_bytes:5000 ~orig_tokens:1250 ~sum_lines:10 ~sum_bytes:500
      ~sum_tokens:125 ~timestamp:"2026-03-11T00:00:00Z"
      ~summary:"This is the summary" ~template:None
  in
  Alcotest.(check bool)
    "contains sum_id" true
    (try
       ignore (Str.search_forward (Str.regexp "sum_test_env000") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains unsummarize hint" true
    (try
       ignore (Str.search_forward (Str.regexp "unsummarize") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains summary text" true
    (try
       ignore (Str.search_forward (Str.regexp "This is the summary") envelope 0);
       true
     with Not_found -> false)

let test_envelope_custom_template () =
  let template =
    Some
      "Summary {sum_id} for {tool_name} (model={model}): \
       {orig_lines}L/{orig_bytes}B -> {sum_lines}L/{sum_bytes}B at {timestamp}\n\
       {summary}"
  in
  let envelope =
    Summarizer.build_envelope ~summary_id:"sum_custom_t001"
      ~tool_name:"file_read" ~model:"groq:test" ~orig_lines:200
      ~orig_bytes:10000 ~orig_tokens:2500 ~sum_lines:20 ~sum_bytes:1000
      ~sum_tokens:250 ~timestamp:"2026-03-11T12:00:00Z"
      ~summary:"Custom summary content" ~template
  in
  Alcotest.(check bool)
    "contains sum_id" true
    (try
       ignore (Str.search_forward (Str.regexp "sum_custom_t001") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains tool_name" true
    (try
       ignore (Str.search_forward (Str.regexp "file_read") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains model" true
    (try
       ignore (Str.search_forward (Str.regexp "groq:test") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains orig_lines" true
    (try
       ignore (Str.search_forward (Str.regexp "200L") envelope 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains summary content" true
    (try
       ignore
         (Str.search_forward (Str.regexp "Custom summary content") envelope 0);
       true
     with Not_found -> false);
  (* Should NOT contain the default envelope markers *)
  Alcotest.(check bool)
    "no default Auto-summarized header" true
    (not
       (try
          ignore (Str.search_forward (Str.regexp "Auto-summarized") envelope 0);
          true
        with Not_found -> false))

(* -- Tool_postprocess tests -- *)

let test_postprocess_passthrough_small () =
  let config = Runtime_config.default in
  let result =
    Lwt_main.run
      (Tool_postprocess.process_tool_result ~config ~db:None ~session_key:None
         ~tool_name:"test" ~history:[] ~raw_result:"small output")
  in
  Alcotest.(check string) "unchanged" "small output" result

let test_postprocess_truncates_when_no_db () =
  let config =
    {
      Runtime_config.default with
      summarizer =
        {
          Runtime_config.default_summarizer_config with
          threshold_chars = 100;
          p2_max_chars = 200;
        };
    }
  in
  let big = String.make 500 'x' in
  let result =
    Lwt_main.run
      (Tool_postprocess.process_tool_result ~config ~db:None
         ~session_key:(Some "test") ~tool_name:"test" ~history:[]
         ~raw_result:big)
  in
  (* Should be truncated to p2_max_chars + truncation message *)
  Alcotest.(check bool) "truncated" true (String.length result < 500);
  Alcotest.(check bool)
    "contains truncation notice" true
    (try
       ignore (Str.search_forward (Str.regexp "truncated") result 0);
       true
     with Not_found -> false)

(* -- Schema migration test -- *)

let test_schema_migration_12_to_13 () =
  let path = Filename.temp_file "test_migrate" ".db" in
  let db = Sqlite3.db_open path in
  ignore (Sqlite3.exec db "PRAGMA busy_timeout = 5000");
  (* Set up schema version 12 *)
  ignore
    (Sqlite3.exec db
       "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL)");
  ignore (Sqlite3.exec db "INSERT INTO schema_version (version) VALUES (12)");
  (* Create required tables that migration expects *)
  ignore
    (Sqlite3.exec db
       "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY, \
        session_key TEXT, role TEXT, content TEXT, tool_call_id TEXT, \
        tool_name TEXT, tool_calls_json TEXT, provider_response_items_json \
        TEXT, created_at TEXT DEFAULT (datetime('now')))");
  ignore
    (Sqlite3.exec db
       "CREATE TABLE IF NOT EXISTS task_tree (id INTEGER PRIMARY KEY, \
        deleted_at TEXT DEFAULT NULL)");
  (* Run the full init_db which handles migration *)
  ignore (Sqlite3.db_close db);
  let db2 = Memory.init ~db_path:path ~search_enabled:false () in
  (* Verify summaries table exists *)
  let has_table =
    let stmt =
      Sqlite3.prepare db2
        "SELECT name FROM sqlite_master WHERE type='table' AND name='summaries'"
    in
    let result =
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () -> Sqlite3.step stmt = Sqlite3.Rc.ROW)
    in
    result
  in
  Alcotest.(check bool) "summaries table exists" true has_table;
  (* Verify schema version is 21 *)
  let version =
    let stmt = Sqlite3.prepare db2 "SELECT version FROM schema_version" in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT n -> Int64.to_int n
            | _ -> -1)
        | _ -> -1)
  in
  Alcotest.(check int) "schema version 29" 29 version;
  ignore (Sqlite3.db_close db2);
  try Unix.unlink path with _ -> ()

(* -- Truncate_for_history tests -- *)

let test_truncate_for_history_short () =
  let s = "short" in
  let result = Summarizer.truncate_for_history s ~max_chars:100 in
  Alcotest.(check string) "unchanged" "short" result

let test_truncate_for_history_long () =
  let s = String.make 200 'x' in
  let result = Summarizer.truncate_for_history s ~max_chars:100 in
  Alcotest.(check bool)
    "truncated to max_chars + notice" true
    (String.length result > 100 && String.length result < 200);
  Alcotest.(check bool)
    "starts with x's" true
    (String.sub result 0 100 = String.make 100 'x')

let suite =
  [
    Alcotest.test_case "pmodel: parse valid" `Quick test_pmodel_parse_valid;
    Alcotest.test_case "pmodel: parse simple" `Quick test_pmodel_parse_simple;
    Alcotest.test_case "pmodel: parse no colon" `Quick
      test_pmodel_parse_no_colon;
    Alcotest.test_case "pmodel: parse empty provider" `Quick
      test_pmodel_parse_empty_provider;
    Alcotest.test_case "pmodel: parse empty model" `Quick
      test_pmodel_parse_empty_model;
    Alcotest.test_case "pmodel: parse_exn valid" `Quick test_pmodel_parse_exn;
    Alcotest.test_case "pmodel: parse_exn invalid" `Quick
      test_pmodel_parse_exn_invalid;
    Alcotest.test_case "pmodel: flexible canonical" `Quick
      test_pmodel_flexible_canonical;
    Alcotest.test_case "pmodel: flexible legacy" `Quick
      test_pmodel_flexible_legacy;
    Alcotest.test_case "pmodel: flexible bare" `Quick test_pmodel_flexible_bare;
    Alcotest.test_case "pmodel: flexible trims whitespace" `Quick
      test_pmodel_flexible_trims;
    Alcotest.test_case "pmodel: flexible to canonical" `Quick
      test_pmodel_flexible_to_canonical;
    Alcotest.test_case "pmodel: deprecation warning primary_model" `Quick
      test_pmodel_deprecation_warning_primary_model;
    Alcotest.test_case "store: round-trip" `Quick test_store_and_find;
    Alcotest.test_case "store: not found" `Quick test_find_not_found;
    Alcotest.test_case "store: delete for session" `Quick
      test_delete_for_session;
    Alcotest.test_case "store: purge TTL" `Quick test_purge_ttl;
    Alcotest.test_case "store: generate_id format" `Quick
      test_generate_id_format;
    Alcotest.test_case "prompt loader: missing file" `Quick
      test_agent_prompt_loader_missing_file;
    Alcotest.test_case "prompt loader: with frontmatter" `Quick
      test_agent_prompt_loader_with_frontmatter;
    Alcotest.test_case "summarizer: passthrough below threshold" `Quick
      test_passthrough_below_threshold;
    Alcotest.test_case "summarizer: passthrough excluded tool" `Quick
      test_passthrough_excluded_tool;
    Alcotest.test_case "summarizer: passthrough disabled" `Quick
      test_passthrough_disabled;
    Alcotest.test_case "summarizer: fallback no db" `Quick test_fallback_no_db;
    Alcotest.test_case "summarizer: fallback no session" `Quick
      test_fallback_no_session;
    Alcotest.test_case "unsummarize: basic" `Quick test_unsummarize_basic;
    Alcotest.test_case "unsummarize: offset" `Quick test_unsummarize_offset;
    Alcotest.test_case "unsummarize: head_and_tail" `Quick
      test_unsummarize_head_and_tail;
    Alcotest.test_case "unsummarize: head_and_tail overlap" `Quick
      test_unsummarize_head_and_tail_overlap;
    Alcotest.test_case "unsummarize: with_context" `Quick
      test_unsummarize_with_context;
    Alcotest.test_case "unsummarize: not found" `Quick
      test_unsummarize_not_found;
    Alcotest.test_case "envelope: default format" `Quick test_envelope_default;
    Alcotest.test_case "envelope: custom template" `Quick
      test_envelope_custom_template;
    Alcotest.test_case "postprocess: passthrough small" `Quick
      test_postprocess_passthrough_small;
    Alcotest.test_case "postprocess: truncates when no db" `Quick
      test_postprocess_truncates_when_no_db;
    Alcotest.test_case "schema migration: 12 to 20" `Quick
      test_schema_migration_12_to_13;
    Alcotest.test_case "truncate: short" `Quick test_truncate_for_history_short;
    Alcotest.test_case "truncate: long" `Quick test_truncate_for_history_long;
  ]
