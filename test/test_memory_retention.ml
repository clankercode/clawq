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
  ]
