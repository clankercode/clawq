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

let fts_matches db word key =
  rows db
    "SELECT rowid FROM messages_fts WHERE messages_fts MATCH ? AND session_key \
     = ?"
    [ Sqlite3.Data.TEXT word; Sqlite3.Data.TEXT key ]
  |> List.length

let with_late_search ~partial f =
  with_provider (fun config _ ->
      let path = Filename.concat config.workspace "late-search.db" in
      let use_db ~search_enabled f =
        let db = Memory.init ~db_path:path ~search_enabled () in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.db_close db))
          (fun () -> f db)
      in
      use_db ~search_enabled:false (fun db ->
          store db raw "legacyBeforeSearch";
          store db "other:unindexed" "untouchedBeforeSearch");
      use_db ~search_enabled:true (fun db ->
          if partial then store db raw "legacyAfterSearch";
          store db "other:indexed" "untouchedAfterSearch";
          f config db))

let test_late_search partial () =
  with_late_search ~partial (fun config db ->
      let before =
        rows db
          "SELECT id,content,created_at FROM messages WHERE session_key = ? \
           ORDER BY id"
          (param raw)
      in
      ignore (Session.create ~config ~db ());
      Alcotest.(check bool)
        "message IDs and payloads survive late search" true
        (before
        = rows db
            "SELECT id,content,created_at FROM messages WHERE session_key = ? \
             ORDER BY id"
            (param canonical));
      Alcotest.(check int)
        "previously unindexed legacy row is searchable" 1
        (fts_matches db "legacyBeforeSearch" canonical);
      if partial then
        Alcotest.(check int)
          "previously indexed legacy row is searchable once" 1
          (fts_matches db "legacyAfterSearch" canonical);
      Alcotest.(check int)
        "old raw key no longer searchable" 0
        (fts_matches db "legacyBeforeSearch" raw);
      Alcotest.(check int)
        "unrelated indexed row retained" 1
        (fts_matches db "untouchedAfterSearch" "other:indexed");
      Alcotest.(check int)
        "no unrelated index rebuild/backfill" 0
        (fts_matches db "untouchedBeforeSearch" "other:unindexed");
      ignore (Session.create ~config ~db ());
      Alcotest.(check int)
        "repeated manager creation leaves one index entry" 1
        (fts_matches db "legacyBeforeSearch" canonical))

let test_late_search_rollback () =
  with_late_search ~partial:true (fun config db ->
      Sql_util.exec_with_params db
        "INSERT INTO pending_questions(session_key,questions_json) VALUES (?, \
         '[]')"
        (param raw);
      Sql_util.exec_exn db
        "CREATE TRIGGER reject_late_search BEFORE UPDATE OF session_key ON \
         pending_questions BEGIN SELECT RAISE(ABORT, 'late search rollback'); \
         END";
      let failure =
        try
          ignore (Session.create ~config ~db ());
          "no failure"
        with exn -> Printexc.to_string exn
      in
      Alcotest.(check bool)
        "failure reaches the late metadata write" true
        (Test_helpers.string_contains failure "pending_questions");
      Alcotest.(check int)
        "unindexed source remains unindexed on rollback" 0
        (fts_matches db "legacyBeforeSearch" raw);
      Alcotest.(check int)
        "indexed source restored on rollback" 1
        (fts_matches db "legacyAfterSearch" raw);
      Alcotest.(check int)
        "no canonical index after rollback" 0
        (fts_matches db "legacyAfterSearch" canonical);
      Alcotest.(check (list string))
        "both source messages retained"
        [ "legacyBeforeSearch"; "legacyAfterSearch" ]
        (current db raw);
      Alcotest.(check int)
        "no committed receipt" 0
        (List.length
           (rows db "SELECT raw_key FROM session_key_reconciliations" [])))

let completion_task db key =
  Background_task.init_schema db;
  Sql_util.exec_with_params db
    "INSERT INTO \
     background_tasks(runner,repo_path,prompt,status,session_key,channel,channel_id,use_worktree,automerge,agent_name,result_preview) \
     VALUES ('local','/scratch','test \
     completion','succeeded',?,'github','acme/backend',0,0,'fixture','finished')"
    (param key);
  let id = Int64.to_int (Sqlite3.last_insert_rowid db) in
  Option.get (Background_task.get_task ~db ~id)

let test_background_reference_only () =
  with_legacy (fun config db ->
      let task = completion_task db raw in
      ignore (Session.create ~config ~db ());
      let restored = Option.get (Background_task.get_task ~db ~id:task.id) in
      Alcotest.(check (option string))
        "parent-only identity discovered and promoted" (Some canonical)
        restored.session_key;
      Alcotest.(check (option string))
        "routing remains original repository" (Some "acme/backend")
        restored.channel_id;
      Alcotest.(check int) "task ID preserved" task.id restored.id)

let test_held_background_completion reset () =
  with_provider (fun config requests ->
      Test_helpers.with_memory_store (fun db ->
          store db raw "preserved raw branch";
          store db canonical "canonical authority";
          let task = completion_task db raw in
          let mgr = Session.create ~config ~db () in
          if reset then ignore (Lwt_main.run (Session.reset mgr ~key:canonical));
          let expected = current db canonical in
          Lwt_main.run
            (let open Lwt.Syntax in
             let* () =
               Daemon_util.inject_background_task_completion
                 ~session_manager:mgr ~config ~session_key:raw task
             in
             Daemon_util.notify_background_task_finished ~session_manager:mgr
               ~config ~db task);
          Alcotest.(check int)
            "held completion never calls provider" 0 (List.length !requests);
          Alcotest.(check (list string))
            "canonical history unchanged" expected (current db canonical);
          Alcotest.(check (list string))
            "held branch retained" [ "preserved raw branch" ] (current db raw);
          Alcotest.(check (option string))
            "held parent reference retained" (Some raw)
            (Option.get (Background_task.get_task ~db ~id:task.id)).session_key))

let test_colliding_background_completions () =
  with_provider (fun config requests ->
      Test_helpers.with_memory_store (fun db ->
          let first = "github:acme/backend:workflow:ci/a"
          and second = "github:acme/backend:workflow:ci_a" in
          let tasks = List.map (completion_task db) [ first; second ] in
          let mgr = Session.create ~config ~db () in
          List.iter
            (fun task ->
              Lwt_main.run
                (Daemon_util.notify_background_task_finished
                   ~session_manager:mgr ~config ~db task))
            tasks;
          Alcotest.(check int)
            "ambiguous parent references never auto-run" 0
            (List.length !requests);
          Alcotest.(check (list string))
            "no history created for canonical collision" []
            (current db (Session.sanitize_session_key first));
          Alcotest.(check (list string))
            "both raw parent references preserved"
            (List.sort String.compare [ first; second ])
            (keys db "background_tasks")))

let test_promoted_background_completion () =
  with_provider (fun config requests ->
      let config =
        { config with observer = { config.observer with enabled = false } }
      in
      Test_helpers.with_memory_store (fun db ->
          store db raw "recovered prefix";
          let stale_task = completion_task db raw in
          let mgr = Session.create ~config ~db () in
          let task =
            Option.get (Background_task.get_task ~db ~id:stale_task.id)
          in
          let notifier_seen = ref false and progress_seen = ref false in
          let canonical_continuation_armed = ref false in
          Session.set_special_command_handler mgr
            (fun ~key ~message:_ ~send_progress ~interrupt_check:_ ->
              notifier_seen :=
                key = canonical
                && Option.is_some (Session.find_registered_notifier mgr ~key);
              progress_seen := Option.is_some send_progress;
              let open Lwt.Syntax in
              let* () =
                match send_progress with
                | Some send -> send "completion progress"
                | None -> Lwt.return_unit
              in
              Lwt.return_none);
          Lwt_main.run
            (let open Lwt.Syntax in
             Lwt.finalize
               (fun () ->
                 let* () =
                   Daemon_util.notify_background_task_finished
                     ~continuation_delay:100.0 ~session_manager:mgr ~config ~db
                     task
                 in
                 (* Also exercise an already-captured raw task after promotion. *)
                 Daemon_util.inject_background_task_completion
                   ~continuation_delay:100.0 ~session_manager:mgr ~config
                   ~session_key:raw ~channel:"github" ~channel_id:"acme/backend"
                   stale_task)
               (fun () ->
                 canonical_continuation_armed :=
                   Option.fold ~none:false
                     ~some:(fun state -> Option.is_some state.Session.cancel)
                     (Hashtbl.find_opt mgr.continuation_checks canonical);
                 let* () =
                   Session.cancel_autonomous_continuation mgr ~key:canonical
                 in
                 if Hashtbl.mem mgr.continuation_checks raw then
                   Session.cancel_autonomous_continuation mgr ~key:raw
                 else Lwt.return_unit));
          Alcotest.(check bool)
            "canonical completion notifier is available" true !notifier_seen;
          Alcotest.(check bool)
            "progress callback is available" true !progress_seen;
          Alcotest.(check (option string))
            "persisted task parent promoted" (Some canonical) task.session_key;
          Alcotest.(check int)
            "both eligible completion paths call provider" 2
            (List.length !requests);
          List.iter
            (fun (key, messages, _) ->
              Alcotest.(check (option string))
                "completion provider key" (Some canonical) key;
              Alcotest.(check int)
                "recovered prefix included once" 1
                (List.length
                   (List.filter
                      (fun (m : Provider.message) ->
                        m.content = "recovered prefix")
                      messages)))
            !requests;
          Alcotest.(check (list string))
            "response state has only canonical ownership" [ canonical ]
            (keys db "session_state");
          Alcotest.(check bool)
            "response marked sent under canonical key" true
            (rows db
               "SELECT response_sent_at FROM session_state WHERE session_key = \
                ? AND response_sent_at IS NOT NULL"
               (param canonical)
            <> []);
          Alcotest.(check bool)
            "canonical continuation was armed" true
            !canonical_continuation_armed;
          Alcotest.(check bool)
            "no raw continuation state" false
            (Hashtbl.mem mgr.continuation_checks raw)))

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
    Alcotest.test_case "late search enablement handles an empty index" `Quick
      (test_late_search false);
    Alcotest.test_case "late search enablement handles a partial index" `Quick
      (test_late_search true);
    Alcotest.test_case "partial index changes roll back with failed promotion"
      `Quick test_late_search_rollback;
    Alcotest.test_case "background parent-only identities are reconciled" `Quick
      test_background_reference_only;
    Alcotest.test_case
      "held background completion cannot enter canonical history" `Quick
      (test_held_background_completion false);
    Alcotest.test_case
      "held background completion cannot resurrect reset history" `Quick
      (test_held_background_completion true);
    Alcotest.test_case
      "colliding background parents cannot wake canonical session" `Quick
      test_colliding_background_completions;
    Alcotest.test_case
      "promoted completion routes notifier progress and state canonically"
      `Quick test_promoted_background_completion;
  ]
