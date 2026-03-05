let test_fts_init () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  ignore db

let test_fts_init_disabled () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  ignore db

let test_search_basic () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user"
       ~content:"Tell me about OCaml programming");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"assistant"
       ~content:"OCaml is a functional programming language");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"What about Python?");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"assistant"
       ~content:"Python is a dynamic language");
  let results = Memory.search ~db ~query:"OCaml" ~limit:5 () in
  Alcotest.(check bool) "found OCaml results" true (List.length results > 0);
  Alcotest.(check bool)
    "results mention OCaml" true
    (List.exists
       (fun (m : Provider.message) ->
         let re = Str.regexp_string "OCaml" in
         try
           ignore (Str.search_forward re m.content 0);
           true
         with Not_found -> false)
       results)

let test_search_session_filter () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"OCaml programming");
  Memory.store_message ~db ~session_key:"s2"
    (Provider.make_message ~role:"user" ~content:"OCaml types");
  let s1_results =
    Memory.search ~db ~query:"OCaml" ~session_key:"s1" ~limit:5 ()
  in
  Alcotest.(check int) "1 result for s1" 1 (List.length s1_results)

let test_search_limit () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Message about topic %d" i))
  done;
  let results = Memory.search ~db ~query:"topic" ~limit:3 () in
  Alcotest.(check bool) "at most 3 results" true (List.length results <= 3)

let test_search_no_results () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Hello world");
  let results = Memory.search ~db ~query:"xyznonexistent" ~limit:5 () in
  Alcotest.(check int) "no results" 0 (List.length results)

let suite =
  [
    Alcotest.test_case "FTS init enabled" `Quick test_fts_init;
    Alcotest.test_case "FTS init disabled" `Quick test_fts_init_disabled;
    Alcotest.test_case "search basic" `Quick test_search_basic;
    Alcotest.test_case "search session filter" `Quick test_search_session_filter;
    Alcotest.test_case "search limit" `Quick test_search_limit;
    Alcotest.test_case "search no results" `Quick test_search_no_results;
  ]
