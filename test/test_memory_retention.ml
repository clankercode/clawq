let make_config ?(max_messages = 500) ?(max_age_days = 30) () =
  {
    Runtime_config.default with
    memory =
      {
        Runtime_config.default.memory with
        max_messages_per_session = max_messages;
        max_message_age_days = max_age_days;
      };
  }

let test_cleanup_by_count () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Message %d" i))
  done;
  let before = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "10 messages before cleanup" 10 (List.length before);
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:5 ~max_age_days:0;
  let after = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "5 messages after cleanup" 5 (List.length after)

let test_cleanup_by_age () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Insert old messages directly via SQL *)
  let insert_old sql_db session_key content age_days =
    let sql =
      Printf.sprintf
        "INSERT INTO messages (session_key, role, content, created_at) VALUES \
         (?, 'user', ?, datetime('now', '-%d days'))"
        age_days
    in
    let stmt = Sqlite3.prepare sql_db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT content));
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt)
  in
  insert_old db "s1" "Old message 1" 60;
  insert_old db "s1" "Old message 2" 45;
  (* Insert a recent message normally *)
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Recent message");
  let before = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "3 messages before cleanup" 3 (List.length before);
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:0 ~max_age_days:1;
  let after = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "1 message after age cleanup" 1 (List.length after);
  Alcotest.(check string)
    "recent message preserved" "Recent message" (List.hd after).content

let test_preserves_newest () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Message %d" i))
  done;
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:3 ~max_age_days:0;
  let after = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "3 messages remain" 3 (List.length after);
  let contents = List.map (fun (m : Provider.message) -> m.content) after in
  Alcotest.(check (list string))
    "newest messages kept"
    [ "Message 8"; "Message 9"; "Message 10" ]
    contents

let test_configurable_max_history () =
  let config = make_config ~max_messages:10 () in
  let agent = Agent.create ~config () in
  (* Add 20 messages to history (stored reversed, newest first) *)
  for i = 1 to 20 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  Alcotest.(check int) "20 messages before trim" 20 (List.length agent.history);
  Agent.trim_history agent;
  Alcotest.(check int) "10 messages after trim" 10 (List.length agent.history)

let test_trim_history_count_only () =
  (* trim_history should only trigger on message count, not tokens.
     With a small number of messages below effective_max, it must be a no-op
     even if those messages are very large (token-based trigger removed). *)
  let config = make_config ~max_messages:50 () in
  let agent = Agent.create ~config () in
  (* Add 5 messages — well below effective_max of 50 *)
  for i = 1 to 5 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  Agent.trim_history agent;
  Alcotest.(check int) "5 messages unchanged" 5 (List.length agent.history)

let test_force_compress_history () =
  let config = make_config () in
  let agent = Agent.create ~config () in
  (* Add 10 messages (newest first) *)
  for i = 1 to 10 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  let compressed = Agent.force_compress_history agent in
  Alcotest.(check bool) "compression performed" true compressed;
  (* force_compress_keep = 4: keep newest 4, which are msgs 10, 9, 8, 7 *)
  Alcotest.(check int) "4 messages remain" 4 (List.length agent.history);
  let contents =
    List.map (fun (m : Provider.message) -> m.content) agent.history
  in
  Alcotest.(check (list string))
    "newest 4 kept"
    [ "Msg 10"; "Msg 9"; "Msg 8"; "Msg 7" ]
    contents

let test_force_compress_history_noop_when_small () =
  let config = make_config () in
  let agent = Agent.create ~config () in
  (* Add only 4 messages — at or below context_recovery_min_history *)
  for i = 1 to 4 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  let compressed = Agent.force_compress_history agent in
  Alcotest.(check bool) "no compression on small history" false compressed;
  Alcotest.(check int) "history unchanged" 4 (List.length agent.history)

let test_trim_history_idempotent () =
  let config = make_config ~max_messages:3 () in
  let agent = Agent.create ~config () in
  for i = 1 to 8 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  Agent.trim_history agent;
  let once = List.map (fun (m : Provider.message) -> m.content) agent.history in
  Agent.trim_history agent;
  let twice =
    List.map (fun (m : Provider.message) -> m.content) agent.history
  in
  Alcotest.(check (list string)) "trim_history is idempotent" once twice

let test_prepare_turn_history_enforces_max_messages () =
  let config = make_config ~max_messages:3 () in
  let agent = Agent.create ~config () in
  for i = 1 to 8 do
    agent.history <-
      Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i)
      :: agent.history
  done;
  ignore
    (Lwt_main.run (Agent.prepare_turn_history agent ~user_message:"latest" ()));
  Alcotest.(check int)
    "history bounded before provider call" 3
    (List.length agent.history)

let test_session_load_trims_history () =
  let db = Memory.init ~db_path:":memory:" () in
  let config = make_config ~max_messages:10 () in
  let session_manager = Session.create ~config ~db () in
  (* Store 30 messages in the DB for session "s1" *)
  for i = 1 to 30 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i))
  done;
  (* Access the session, which triggers history load + trim *)
  let agent_history_len =
    Lwt_main.run
      (Session.with_session_lock session_manager ~key:"s1"
         (fun agent _interrupt -> Lwt.return (List.length agent.Agent.history)))
  in
  (* effective_max = min max_messages 500 = 10, so history should be trimmed *)
  Alcotest.(check bool) "history trimmed on load" true (agent_history_len <= 10)

let test_replace_session_messages () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Store 10 original messages *)
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Original %d" i))
  done;
  let before = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "10 messages before replace" 10 (List.length before);
  (* Replace with 3 compacted messages *)
  let compacted =
    [
      Provider.make_message ~role:"assistant" ~content:"[Summary of history]";
      Provider.make_message ~role:"user" ~content:"Recent question";
      Provider.make_message ~role:"assistant" ~content:"Recent answer";
    ]
  in
  Memory.replace_session_messages ~db ~session_key:"s1" compacted;
  let after = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "3 messages after replace" 3 (List.length after);
  Alcotest.(check string)
    "first is summary" "[Summary of history]" (List.hd after).content;
  (* Verify other sessions are unaffected *)
  Memory.store_message ~db ~session_key:"s2"
    (Provider.make_message ~role:"user" ~content:"Other session");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [ Provider.make_message ~role:"user" ~content:"Only one" ];
  let s1 = Memory.load_history ~db ~session_key:"s1" in
  let s2 = Memory.load_history ~db ~session_key:"s2" in
  Alcotest.(check int) "s1 replaced" 1 (List.length s1);
  Alcotest.(check int) "s2 untouched" 1 (List.length s2)

let test_cleanup_all () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 8 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "S1 msg %d" i))
  done;
  for i = 1 to 6 do
    Memory.store_message ~db ~session_key:"s2"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "S2 msg %d" i))
  done;
  Memory.cleanup_all ~db ~max_messages:3 ~max_age_days:0;
  let s1 = Memory.load_history ~db ~session_key:"s1" in
  let s2 = Memory.load_history ~db ~session_key:"s2" in
  Alcotest.(check int) "s1 has 3 messages" 3 (List.length s1);
  Alcotest.(check int) "s2 has 3 messages" 3 (List.length s2)

let make_assistant_with_tool_calls calls =
  {
    Provider.role = "assistant";
    content = "";
    content_parts = [];
    tool_calls = calls;
    tool_call_id = None;
    name = None;
    provider_response_items_json = None;
    thinking = None;
    is_error = false;
  }

let make_tool_call id name =
  { Provider.id; function_name = name; arguments = "{}" }

let make_tool_result_msg id name =
  Provider.make_tool_result ~tool_call_id:id ~name ~content:"result"

let test_adjust_split_moves_orphan_tool_results () =
  let tc = make_tool_call "tc1" "shell_exec" in
  let assistant = make_assistant_with_tool_calls [ tc ] in
  let tool_result = make_tool_result_msg "tc1" "shell_exec" in
  let user_msg = Provider.make_message ~role:"user" ~content:"hello" in
  (* Split lands between assistant+tool_calls and its tool result *)
  let to_compact = [ assistant ] in
  let to_keep = [ tool_result; user_msg ] in
  let compact', keep' = Agent.adjust_split_for_tool_groups to_compact to_keep in
  Alcotest.(check int) "orphan moved to compact" 2 (List.length compact');
  Alcotest.(check int) "keep has only user msg" 1 (List.length keep');
  Alcotest.(check string)
    "kept msg is user" "user" (List.hd keep').Provider.role

let test_adjust_split_noop_when_clean () =
  let tc = make_tool_call "tc1" "shell_exec" in
  let assistant = make_assistant_with_tool_calls [ tc ] in
  let tool_result = make_tool_result_msg "tc1" "shell_exec" in
  let user_msg = Provider.make_message ~role:"user" ~content:"hello" in
  (* Clean split: tool group is entirely in to_compact *)
  let to_compact = [ assistant; tool_result ] in
  let to_keep = [ user_msg ] in
  let compact', keep' = Agent.adjust_split_for_tool_groups to_compact to_keep in
  Alcotest.(check int) "compact unchanged" 2 (List.length compact');
  Alcotest.(check int) "keep unchanged" 1 (List.length keep')

let test_ensure_integrity_removes_orphaned_results () =
  let orphan = make_tool_result_msg "tc_missing" "file_read" in
  let user_msg = Provider.make_message ~role:"user" ~content:"hi" in
  let msgs = [ user_msg; orphan ] in
  let fixed = Agent.ensure_tool_group_integrity msgs in
  Alcotest.(check int) "orphaned tool result removed" 1 (List.length fixed);
  Alcotest.(check string) "remaining is user msg" "user" (List.hd fixed).role

let test_ensure_integrity_strips_dangling_calls () =
  (* B620: when an assistant turn has tool_calls but no matching tool_results
     AND no other content (no text, no thinking, no provider_response_items),
     the post-strip message is empty and Anthropic rejects it ("text content
     blocks must be non-empty"). ensure_tool_group_integrity drops the empty
     assistant. *)
  let tc = make_tool_call "tc1" "shell_exec" in
  let assistant = make_assistant_with_tool_calls [ tc ] in
  let user_msg = Provider.make_message ~role:"user" ~content:"hi" in
  (* No tool result for tc1 *)
  let msgs = [ assistant; user_msg ] in
  let fixed = Agent.ensure_tool_group_integrity msgs in
  Alcotest.(check int)
    "empty assistant dropped, only user remains" 1 (List.length fixed);
  Alcotest.(check string) "remaining is user" "user" (List.hd fixed).role

(* B620 round 3: when an empty-after-strip assistant DOES carry Codex
   provider_response_items_json (e.g., reasoning output items), the message
   must be preserved so Codex can replay those items. *)
let test_ensure_integrity_preserves_assistant_with_provider_items () =
  let tc = make_tool_call "tc-x" "shell_exec" in
  let assistant_with_items =
    {
      (make_assistant_with_tool_calls [ tc ]) with
      provider_response_items_json =
        Some {|[{"type":"reasoning","content":"thinking through the request"}]|};
    }
  in
  let user_msg = Provider.make_message ~role:"user" ~content:"hi" in
  let msgs = [ assistant_with_items; user_msg ] in
  let fixed = Agent.ensure_tool_group_integrity msgs in
  Alcotest.(check int)
    "assistant preserved when provider items remain" 2 (List.length fixed);
  let fixed_assistant = List.hd fixed in
  Alcotest.(check int)
    "tool_calls stripped from preserved assistant" 0
    (List.length fixed_assistant.Provider.tool_calls);
  Alcotest.(check bool)
    "provider_response_items_json intact" true
    (fixed_assistant.Provider.provider_response_items_json <> None)

let test_ensure_integrity_preserves_complete_groups () =
  let tc = make_tool_call "tc1" "shell_exec" in
  let assistant = make_assistant_with_tool_calls [ tc ] in
  let tool_result = make_tool_result_msg "tc1" "shell_exec" in
  let user_msg = Provider.make_message ~role:"user" ~content:"hi" in
  let msgs = [ user_msg; assistant; tool_result ] in
  let fixed = Agent.ensure_tool_group_integrity msgs in
  Alcotest.(check int) "all 3 messages preserved" 3 (List.length fixed);
  let fixed_assistant = List.nth fixed 1 in
  Alcotest.(check int)
    "tool_calls preserved" 1
    (List.length fixed_assistant.Provider.tool_calls)

let test_trim_history_tool_group_integrity () =
  let config = make_config ~max_messages:3 () in
  let agent = Agent.create ~config () in
  (* Build history (newest-first): user, tool_result, assistant+tool_calls, ...old *)
  let tc = make_tool_call "tc1" "shell_exec" in
  let old_msgs =
    List.init 5 (fun i ->
        Provider.make_message ~role:"user" ~content:(Printf.sprintf "old %d" i))
  in
  (* History is newest-first, so: tool_result, assistant, then old msgs *)
  agent.history <-
    [
      make_tool_result_msg "tc1" "shell_exec";
      make_assistant_with_tool_calls [ tc ];
    ]
    @ old_msgs;
  Agent.trim_history agent;
  (* After trim to 3 + integrity fix, no orphaned tool results should remain *)
  List.iter
    (fun (m : Provider.message) ->
      if m.role = "tool" then
        match m.tool_call_id with
        | Some id ->
            let has_call =
              List.exists
                (fun (m2 : Provider.message) ->
                  m2.role = "assistant"
                  && List.exists
                       (fun (tc : Provider.tool_call) -> tc.id = id)
                       m2.tool_calls)
                agent.history
            in
            Alcotest.(check bool) "tool result has matching call" true has_call
        | None -> ()
      else ())
    agent.history

let test_force_compress_tool_group_integrity () =
  let config = make_config () in
  let agent = Agent.create ~config () in
  let tc = make_tool_call "tc1" "shell_exec" in
  (* Build 10 messages: newest first = tool_result, assistant+tc, then 8 user msgs *)
  let old_msgs =
    List.init 8 (fun i ->
        Provider.make_message ~role:"user" ~content:(Printf.sprintf "Msg %d" i))
  in
  agent.history <-
    [
      make_tool_result_msg "tc1" "shell_exec";
      make_assistant_with_tool_calls [ tc ];
    ]
    @ old_msgs;
  let compressed = Agent.force_compress_history agent in
  Alcotest.(check bool) "compression performed" true compressed;
  (* Verify no orphaned tool results *)
  List.iter
    (fun (m : Provider.message) ->
      if m.role = "tool" then
        match m.tool_call_id with
        | Some id ->
            let has_call =
              List.exists
                (fun (m2 : Provider.message) ->
                  m2.role = "assistant"
                  && List.exists
                       (fun (tc : Provider.tool_call) -> tc.id = id)
                       m2.tool_calls)
                agent.history
            in
            Alcotest.(check bool) "tool result has matching call" true has_call
        | None -> ()
      else ())
    agent.history

let test_cleanup_session_preserves_tool_groups () =
  let db = Memory.init ~db_path:":memory:" () in
  let tc = make_tool_call "tc1" "shell_exec" in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"old");
  Memory.store_message ~db ~session_key:"s1"
    (make_assistant_with_tool_calls [ tc ]);
  Memory.store_message ~db ~session_key:"s1"
    (make_tool_result_msg "tc1" "shell_exec");
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:1 ~max_age_days:0;
  let after = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "keeps complete tool group" 2 (List.length after);
  Alcotest.(check string) "assistant kept" "assistant" (List.hd after).role;
  Alcotest.(check string) "tool kept" "tool" (List.nth after 1).role

let suite =
  [
    Alcotest.test_case "cleanup by count" `Quick test_cleanup_by_count;
    Alcotest.test_case "cleanup by age" `Quick test_cleanup_by_age;
    Alcotest.test_case "preserves newest" `Quick test_preserves_newest;
    Alcotest.test_case "configurable max_history" `Quick
      test_configurable_max_history;
    Alcotest.test_case "session load trims history" `Quick
      test_session_load_trims_history;
    Alcotest.test_case "replace session messages" `Quick
      test_replace_session_messages;
    Alcotest.test_case "cleanup_all" `Quick test_cleanup_all;
    Alcotest.test_case "trim_history count only" `Quick
      test_trim_history_count_only;
    Alcotest.test_case "force_compress_history" `Quick
      test_force_compress_history;
    Alcotest.test_case "force_compress_history noop when small" `Quick
      test_force_compress_history_noop_when_small;
    Alcotest.test_case "trim_history idempotent" `Quick
      test_trim_history_idempotent;
    Alcotest.test_case "prepare_turn_history enforces max messages" `Quick
      test_prepare_turn_history_enforces_max_messages;
    Alcotest.test_case "adjust_split moves orphan tool results" `Quick
      test_adjust_split_moves_orphan_tool_results;
    Alcotest.test_case "adjust_split noop when clean" `Quick
      test_adjust_split_noop_when_clean;
    Alcotest.test_case "ensure_integrity removes orphaned results" `Quick
      test_ensure_integrity_removes_orphaned_results;
    Alcotest.test_case "ensure_integrity strips dangling calls" `Quick
      test_ensure_integrity_strips_dangling_calls;
    Alcotest.test_case "ensure_integrity preserves complete groups" `Quick
      test_ensure_integrity_preserves_complete_groups;
    Alcotest.test_case
      "B620: ensure_integrity preserves assistant with provider items" `Quick
      test_ensure_integrity_preserves_assistant_with_provider_items;
    Alcotest.test_case "trim_history tool group integrity" `Quick
      test_trim_history_tool_group_integrity;
    Alcotest.test_case "force_compress tool group integrity" `Quick
      test_force_compress_tool_group_integrity;
    Alcotest.test_case "cleanup session preserves tool groups" `Quick
      test_cleanup_session_preserves_tool_groups;
  ]
