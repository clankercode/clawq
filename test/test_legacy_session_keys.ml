open Test_session_key_canonicalization

let param key = [ Sqlite3.Data.TEXT key ]

let store db key content =
  Memory.store_message ~db ~session_key:key
    (Provider.make_message ~role:"user" ~content)

let current db key =
  Memory.load_history ~db ~session_key:key
  |> List.map (fun (m : Provider.message) -> m.content)

let with_legacy f =
  with_provider (fun config _ ->
      Test_helpers.with_memory_store ~search_enabled:true (fun db ->
          f config db))

let load mgr key =
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun agent _ ->
         Lwt.return (List.rev agent.Agent.history)))

let test_promote () =
  with_legacy (fun config db ->
      let message =
        {
          (Provider.make_message ~role:"user" ~content:"uniqueLegacy") with
          thinking = Some "thinking payload";
          provider_response_items_json = Some "[{\"opaque\":true}]";
        }
      in
      Memory.store_message ~db ~session_key:raw message;
      Sql_util.exec_with_params db
        "UPDATE messages SET created_at = '2025-01-01 01:02:03' WHERE \
         session_key = ?"
        (param raw);
      let snapshot key =
        rows db
          "SELECT id, role, content, tool_calls_json, \
           provider_response_items_json, thinking_content, created_at FROM \
           messages WHERE session_key = ? ORDER BY id"
          (param key)
      in
      let original = snapshot raw in
      let mgr = Session.create ~config ~db () in
      Alcotest.(check (list string))
        "raw history restored lazily" [ "uniqueLegacy" ]
        (List.map
           (fun (m : Provider.message) -> m.content)
           (load mgr canonical));
      Alcotest.(check bool)
        "IDs timestamps and opaque payloads preserved" true
        (original = snapshot canonical);
      Alcotest.(check (list string))
        "raw rows moved, not copied" [] (current db raw);
      let matched =
        rows db
          "SELECT rowid FROM messages_fts WHERE messages_fts MATCH \
           'uniqueLegacy' AND session_key = ?"
          (param canonical)
      in
      Alcotest.(check int) "FTS finds canonical key" 1 (List.length matched);
      Alcotest.(check int)
        "FTS has no stale raw index" 0
        (List.length
           (rows db
              "SELECT rowid FROM messages_fts WHERE messages_fts MATCH \
               'uniqueLegacy' AND session_key = ?"
              (param raw)));
      let again = Session.create ~config ~db () in
      Alcotest.(check int)
        "manager recreation does not duplicate history" 1
        (List.length (load again raw)))

let test_settings_and_metadata () =
  with_legacy (fun config db ->
      store db raw "legacy work";
      Memory.set_session_model_override ~db ~session_key:canonical
        ~model:"openai:canonical-model";
      Memory.set_session_model_override ~db ~session_key:raw
        ~model:"openai:legacy-model";
      Memory.upsert_session_state ~db ~session_key:raw ~turn:"agent"
        ~channel:"github" ~channel_id:"acme/backend" ();
      Memory.store_session_workspace_state ~db ~session_key:raw
        ~observed_active_workspace_files:[ ("AGENTS.md", Some "digest") ];
      let queue_id =
        Memory.queue_enqueue ~db ~session_key:raw ~source:"test"
          ~payload_json:"{\"message\":\"queued\"}"
      in
      Sql_util.exec_with_params db
        "INSERT INTO pending_questions(session_key, questions_json) VALUES (?, \
         '[]')"
        (param raw);
      Sql_util.exec_with_params db
        "INSERT INTO session_repos(session_key, local_path) VALUES (?, \
         '/scratch/repo')"
        (param raw);
      Vector.init_schema db;
      Sql_util.exec_with_params db
        "INSERT INTO \
         embeddings(message_id,session_key,content_preview,embedding) SELECT \
         id,session_key,content,X'00' FROM messages WHERE session_key = ?"
        (param raw);
      let refs_before =
        rows db "SELECT message_id FROM embeddings WHERE session_key = ?"
          (param raw)
      in
      let question_ids =
        rows db "SELECT id FROM pending_questions WHERE session_key = ?"
          (param raw)
      in
      ignore (Session.create ~config ~db ());
      Alcotest.(check bool)
        "pending question ID unchanged" true
        (question_ids
        = rows db "SELECT id FROM pending_questions WHERE session_key = ?"
            (param canonical));
      Alcotest.(check (list string))
        "settings-only row does not block history" [ "legacy work" ]
        (current db canonical);
      Alcotest.(check (option string))
        "canonical model wins" (Some "openai:canonical-model")
        (Memory.get_session_model_override ~db ~session_key:canonical);
      Alcotest.(check (option (pair string string)))
        "routing tuple transferred together"
        (Some ("github", "acme/backend"))
        (Memory.get_session_channel ~db ~session_key:canonical);
      Alcotest.(check int)
        "one resumable work record" 1
        (List.length
           (Memory.load_pending_agent_sessions ~db ~max_age_seconds:3600));
      Alcotest.(check (list string))
        "raw live work record retired" [ canonical ] (keys db "session_state");
      List.iter
        (fun table ->
          Alcotest.(check (list string))
            (table ^ " relabeled") [ canonical ] (keys db table))
        [
          "session_workspace_state";
          "inbound_queue";
          "pending_questions";
          "session_repos";
          "embeddings";
        ];
      Alcotest.(check bool)
        "embedding message reference unchanged" true
        (refs_before
        = rows db "SELECT message_id FROM embeddings WHERE session_key = ?"
            (param canonical));
      Alcotest.(check int)
        "queue ID unchanged" queue_id
        (List.hd (Memory.queue_list ~db ~session_key:canonical)).Memory.queue_id)

let test_epochs_and_repair () =
  with_legacy (fun config db ->
      store db raw "older epoch";
      let call =
        { Provider.id = "call"; function_name = "test"; arguments = "{}" }
      in
      let assistant =
        {
          (Provider.make_message ~role:"assistant" ~content:"") with
          tool_calls = [ call ];
        }
      in
      Memory.replace_session_messages ~db ~session_key:raw
        [
          assistant;
          Provider.make_tool_result ~tool_call_id:"call" ~name:"test"
            ~content:"complete group";
          Provider.make_tool_result ~tool_call_id:"orphan" ~name:"test"
            ~content:"orphan";
        ];
      let epoch_ids =
        rows db "SELECT id FROM session_log_epochs WHERE session_key = ?"
          (param raw)
      in
      let mgr = Session.create ~config ~db () in
      Alcotest.(check bool)
        "epoch IDs retained" true
        (epoch_ids
        = rows db "SELECT id FROM session_log_epochs WHERE session_key = ?"
            (param canonical));
      let loaded = load mgr raw in
      Alcotest.(check bool)
        "complete tool group restored" true
        (List.exists
           (fun (m : Provider.message) -> m.content = "complete group")
           loaded);
      Alcotest.(check bool)
        "ordinary orphan repair still applied" false
        (List.exists
           (fun (m : Provider.message) -> m.content = "orphan")
           loaded))

let test_canonical_wins_and_reset () =
  with_legacy (fun config db ->
      store db raw "competing raw branch";
      store db canonical "authoritative canonical branch";
      Memory.upsert_session_state ~db ~session_key:raw ~turn:"agent"
        ~channel:"github" ~channel_id:"acme/backend" ();
      ignore
        (Memory.queue_enqueue ~db ~session_key:raw ~source:"test"
           ~payload_json:"{}");
      Memory.set_session_keepalive ~db ~session_key:raw ~enabled:true;
      Memory.set_session_heartbeat ~db ~session_key:raw ~enabled:true;
      let mgr = Session.create ~config ~db () in
      Alcotest.(check (list string))
        "canonical branch is not merged"
        [ "authoritative canonical branch" ]
        (List.map (fun (m : Provider.message) -> m.content) (load mgr raw));
      Alcotest.(check (list string))
        "raw branch preserved for recovery" [ "competing raw branch" ]
        (current db raw);
      Alcotest.(check int)
        "raw pending work not auto-resumed" 0
        (List.length
           (Memory.load_pending_agent_sessions ~db ~max_age_seconds:3600));
      Alcotest.(check (list string))
        "raw queued work not replayed" []
        (Memory.queue_list_pending_sessions ~db);
      Alcotest.(check (list string))
        "raw keepalive not run" []
        (Memory.list_keepalive_session_keys ~db);
      Alcotest.(check (list string))
        "raw heartbeat not run" []
        (Memory.list_heartbeat_session_keys ~db);
      Alcotest.(check bool)
        "held queue cannot be claimed" true
        (match Memory.queue_claim ~db ~session_key:raw with
        | Memory.Claim_empty -> true
        | Memory.Claim_ok _ -> false);
      ignore (Lwt_main.run (Session.reset mgr ~key:canonical));
      let again = Session.create ~config ~db () in
      Alcotest.(check int)
        "reset does not resurrect competing history" 0
        (List.length (load again raw));
      Alcotest.(check (list string))
        "reset retains preserved raw branch" [ "competing raw branch" ]
        (current db raw))

let test_collision () =
  with_legacy (fun config db ->
      let first = "github:acme/backend:workflow:ci/a"
      and second = "github:acme/backend:workflow:ci_a" in
      List.iter
        (fun key ->
          store db key key;
          Memory.upsert_session_state ~db ~session_key:key ~turn:"agent"
            ~channel:"github" ~channel_id:"acme/backend" ();
          ignore
            (Memory.queue_enqueue ~db ~session_key:key ~source:"test"
               ~payload_json:"{}"))
        [ first; second ];
      let mgr = Session.create ~config ~db () in
      Alcotest.(check int)
        "ambiguous aliases are not adopted" 0
        (List.length (load mgr (Session.sanitize_session_key first)));
      List.iter
        (fun key ->
          Alcotest.(check (list string))
            "each raw branch preserved" [ key ] (current db key))
        [ first; second ];
      Alcotest.(check int)
        "neither alias resumed" 0
        (List.length
           (Memory.load_pending_agent_sessions ~db ~max_age_seconds:3600));
      Alcotest.(check (list string))
        "neither alias replayed" []
        (Memory.queue_list_pending_sessions ~db);
      Alcotest.(check int)
        "diagnostic recorded for both aliases" 2
        (List.length
           (rows db
              "SELECT raw_key FROM session_key_reconciliations WHERE decision \
               = 'deferred'"
              [])))

let test_routing_conflict () =
  with_legacy (fun config db ->
      store db raw "legacy";
      Memory.upsert_session_state ~db ~session_key:raw ~turn:"agent"
        ~channel:"github" ~channel_id:"acme/backend" ();
      Memory.upsert_session_state ~db ~session_key:canonical ~turn:"user"
        ~channel:"slack" ~channel_id:"private-room" ();
      let mgr = Session.create ~config ~db () in
      Alcotest.(check int)
        "different routing blocks adoption" 0
        (List.length (load mgr canonical));
      Alcotest.(check (list string))
        "legacy preserved" [ "legacy" ] (current db raw);
      Alcotest.(check (option (pair string string)))
        "canonical routing retained"
        (Some ("slack", "private-room"))
        (Memory.get_session_channel ~db ~session_key:canonical))

let test_reset_evidence () =
  with_legacy (fun config db ->
      store db canonical "intentionally reset";
      Memory.archive_session ~db ~session_key:canonical;
      Memory.clear_session ~db ~session_key:canonical;
      store db raw "stale raw";
      let mgr = Session.create ~config ~db () in
      Alcotest.(check int)
        "archive evidence prevents stale recovery" 0
        (List.length (load mgr canonical));
      Alcotest.(check (list string))
        "stale raw branch remains inspectable" [ "stale raw" ] (current db raw))

let test_rollback () =
  with_legacy (fun config db ->
      store db raw "keep on rollback";
      Memory.upsert_session_state ~db ~session_key:raw ~turn:"agent"
        ~channel:"github" ~channel_id:"acme/backend" ();
      Memory.set_session_model_override ~db ~session_key:canonical
        ~model:"openai:chosen";
      Sql_util.exec_with_params db
        "INSERT INTO pending_questions(session_key,questions_json) VALUES (?, \
         '[]')"
        (param raw);
      let original_state =
        rows db "SELECT * FROM session_state ORDER BY session_key" []
      in
      Sql_util.exec_exn db
        "CREATE TRIGGER reject_relabel BEFORE UPDATE OF session_key ON \
         pending_questions BEGIN SELECT RAISE(ABORT, 'injected relabel \
         failure'); END";
      let failed =
        try
          ignore (Session.create ~config ~db ());
          false
        with _ -> true
      in
      Alcotest.(check bool) "migration failure is surfaced" true failed;
      Alcotest.(check (list string))
        "history rolled back" [ "keep on rollback" ] (current db raw);
      Alcotest.(check bool)
        "both state identities rolled back" true
        (original_state
        = rows db "SELECT * FROM session_state ORDER BY session_key" []);
      Alcotest.(check int)
        "FTS rollback retains source index" 1
        (List.length
           (rows db
              "SELECT rowid FROM messages_fts WHERE messages_fts MATCH \
               'rollback' AND session_key = ?"
              (param raw)));
      Alcotest.(check (list string))
        "no partial canonical rows" [] (current db canonical);
      Alcotest.(check int)
        "no partial reconciliation decision" 0
        (List.length
           (rows db "SELECT raw_key FROM session_key_reconciliations" [])))

let test_non_github_untouched () =
  with_legacy (fun config db ->
      store db "other:raw/key" "other connector";
      ignore (Session.create ~config ~db ());
      Alcotest.(check (list string))
        "legacy migration limited to GitHub" [ "other connector" ]
        (current db "other:raw/key"))

let test_reappeared_raw_history () =
  with_legacy (fun config db ->
      store db raw "original";
      ignore (Session.create ~config ~db ());
      store db raw "late raw branch";
      Memory.upsert_session_state ~db ~session_key:raw ~turn:"agent"
        ~channel:"github" ~channel_id:"acme/backend" ();
      ignore (Session.create ~config ~db ());
      Alcotest.(check (list string))
        "prior promotion is not repeated" [ "original" ] (current db canonical);
      Alcotest.(check (list string))
        "late branch preserved" [ "late raw branch" ] (current db raw);
      Alcotest.(check int)
        "late raw work does not resume" 0
        (List.length
           (Memory.load_pending_agent_sessions ~db ~max_age_seconds:3600)))

let suite =
  [
    Alcotest.test_case "unique raw history promotes with IDs payloads and FTS"
      `Quick test_promote;
    Alcotest.test_case "canonical settings win while work metadata moves" `Quick
      test_settings_and_metadata;
    Alcotest.test_case "epochs move and normal tool-group repair applies" `Quick
      test_epochs_and_repair;
    Alcotest.test_case "canonical wins without merge or reset resurrection"
      `Quick test_canonical_wins_and_reset;
    Alcotest.test_case "colliding aliases stay separate and cannot auto-run"
      `Quick test_collision;
    Alcotest.test_case "conflicting routing prevents legacy adoption" `Quick
      test_routing_conflict;
    Alcotest.test_case "archive evidence prevents resurrection after reset"
      `Quick test_reset_evidence;
    Alcotest.test_case "failed relabel rolls back all changes" `Quick
      test_rollback;
    Alcotest.test_case "other connector legacy keys remain untouched" `Quick
      test_non_github_untouched;
    Alcotest.test_case "raw data after promotion is held without reimport"
      `Quick test_reappeared_raw_history;
  ]
