let make_config ?(max_messages = 500) ?(max_age_days = 30) () =
  { Runtime_config.default with
    memory = { Runtime_config.default.memory with
      max_messages_per_session = max_messages;
      max_message_age_days = max_age_days;
    }
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
        "INSERT INTO messages (session_key, role, content, created_at) \
         VALUES (?, 'user', ?, datetime('now', '-%d days'))" age_days
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
  Alcotest.(check string) "recent message preserved" "Recent message"
    (List.hd after).content

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
  Alcotest.(check (list string)) "newest messages kept"
    ["Message 8"; "Message 9"; "Message 10"] contents

let test_configurable_max_history () =
  let config = make_config ~max_messages:10 () in
  let agent = Agent.create ~config () in
  (* Add 20 messages to history (stored reversed, newest first) *)
  for i = 1 to 20 do
    agent.history <-
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Msg %d" i))
      :: agent.history
  done;
  Alcotest.(check int) "20 messages before trim" 20 (List.length agent.history);
  Agent.trim_history agent;
  Alcotest.(check int) "10 messages after trim" 10 (List.length agent.history)

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
    Alcotest.test_case "configurable max_history" `Quick test_configurable_max_history;
    Alcotest.test_case "cleanup_all" `Quick test_cleanup_all;
  ]
