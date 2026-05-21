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

(* B654: queries containing colons (FTS5 column-qualifier syntax) must not
   throw "no such column: rig". Our core memory keys frequently use colons
   like "rig:briefing:config", so callers commonly query for them verbatim. *)
let test_recall_core_with_colon_query () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_core ~db ~key:"rig:briefing:config"
    ~content:"{\"topics\":[\"crypto\"]}" ~category:"rig" ();
  Memory.store_core ~db ~key:"lesson:briefing:2026"
    ~content:"avoid empty queries" ~category:"lessons" ();
  let results = Memory.recall_core ~db ~query:"rig:briefing:config" ~limit:5 in
  Alcotest.(check bool)
    "recall finds colon-keyed memory" true
    (List.exists (fun (k, _, _) -> k = "rig:briefing:config") results)

let test_recall_core_with_multi_token_query () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_core ~db ~key:"k1" ~content:"briefing daily news topic" ();
  Memory.store_core ~db ~key:"k2" ~content:"unrelated content" ();
  let results = Memory.recall_core ~db ~query:"briefing news" ~limit:5 in
  Alcotest.(check bool)
    "multi-token AND match" true
    (List.exists (fun (k, _, _) -> k = "k1") results);
  Alcotest.(check bool)
    "non-matching record excluded" false
    (List.exists (fun (k, _, _) -> k = "k2") results)

let test_search_with_colon_query () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user"
       ~content:"please load rig:briefing:config from memory");
  let results = Memory.search ~db ~query:"rig:briefing:config" ~limit:5 () in
  Alcotest.(check bool)
    "colon-query search returns row" true
    (List.length results > 0)

let suite =
  [
    Alcotest.test_case "FTS init enabled" `Quick test_fts_init;
    Alcotest.test_case "FTS init disabled" `Quick test_fts_init_disabled;
    Alcotest.test_case "search basic" `Quick test_search_basic;
    Alcotest.test_case "search session filter" `Quick test_search_session_filter;
    Alcotest.test_case "search limit" `Quick test_search_limit;
    Alcotest.test_case "search no results" `Quick test_search_no_results;
    Alcotest.test_case "B654 recall_core colon query" `Quick
      test_recall_core_with_colon_query;
    Alcotest.test_case "B654 recall_core multi-token query" `Quick
      test_recall_core_with_multi_token_query;
    Alcotest.test_case "B654 search colon query" `Quick
      test_search_with_colon_query;
  ]
