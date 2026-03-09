let test_search_current_messages () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Tell me about OCaml");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"assistant"
       ~content:"OCaml is a functional language");
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"OCaml" ~limit:10
      ()
  in
  Alcotest.(check bool) "found results" true (List.length results > 0);
  List.iter
    (fun (r : Memory.history_search_result) ->
      Alcotest.(check string) "source is current" "current" r.source)
    results

let test_search_archived_epoch () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Old archived content xyz123");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"assistant" ~content:"Acknowledged xyz123");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [
      Provider.make_message ~role:"user" ~content:"New current message";
      Provider.make_message ~role:"assistant" ~content:"New response";
    ];
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"xyz123"
      ~limit:10 ()
  in
  Alcotest.(check bool) "found archived results" true (List.length results > 0);
  List.iter
    (fun (r : Memory.history_search_result) ->
      let is_epoch =
        String.length r.source >= 6 && String.sub r.source 0 6 = "epoch:"
      in
      Alcotest.(check bool) "source is epoch" true is_epoch)
    results

let test_search_both_current_and_archived () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Discuss alpha topic");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [ Provider.make_message ~role:"user" ~content:"More about alpha topic" ];
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"alpha" ~limit:10
      ()
  in
  Alcotest.(check bool) "found multiple results" true (List.length results >= 2);
  let sources =
    List.map (fun (r : Memory.history_search_result) -> r.source) results
  in
  let has_current = List.mem "current" sources in
  let has_epoch =
    List.exists
      (fun s -> String.length s >= 6 && String.sub s 0 6 = "epoch:")
      sources
  in
  Alcotest.(check bool) "has current result" true has_current;
  Alcotest.(check bool) "has archived result" true has_epoch

let test_session_isolation () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Secret message for s1");
  Memory.store_message ~db ~session_key:"s2"
    (Provider.make_message ~role:"user" ~content:"Secret message for s2");
  let s1_results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"Secret"
      ~limit:10 ()
  in
  let s2_results =
    Memory.search_session_history ~db ~session_key:"s2" ~query:"Secret"
      ~limit:10 ()
  in
  Alcotest.(check int) "s1 sees 1 result" 1 (List.length s1_results);
  Alcotest.(check int) "s2 sees 1 result" 1 (List.length s2_results);
  Alcotest.(check bool)
    "s1 result contains s1" true
    (let r = List.hd s1_results in
     try
       ignore (Str.search_forward (Str.regexp_string "s1") r.content 0);
       true
     with Not_found -> false)

let test_no_results () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"Hello world");
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"xyznonexistent"
      ~limit:10 ()
  in
  Alcotest.(check int) "no results" 0 (List.length results)

let test_limit_respected () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  for i = 1 to 20 do
    Memory.store_message ~db ~session_key:"s1"
      (Provider.make_message ~role:"user"
         ~content:(Printf.sprintf "Message about widgets %d" i))
  done;
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"widgets"
      ~limit:5 ()
  in
  Alcotest.(check bool) "at most 5 results" true (List.length results <= 5)

let test_works_without_fts () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"LIKE fallback test content");
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"fallback"
      ~limit:10 ()
  in
  Alcotest.(check bool) "found via LIKE" true (List.length results > 0);
  Alcotest.(check string) "source is current" "current" (List.hd results).source

let test_works_with_fts () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"FTS enabled search test");
  let results =
    Memory.search_session_history ~db ~session_key:"s1" ~query:"enabled"
      ~limit:10 ()
  in
  Alcotest.(check bool) "found via FTS" true (List.length results > 0);
  Alcotest.(check string) "source is current" "current" (List.hd results).source

let suite =
  [
    Alcotest.test_case "search current messages" `Quick
      test_search_current_messages;
    Alcotest.test_case "search archived epoch" `Quick test_search_archived_epoch;
    Alcotest.test_case "search both current and archived" `Quick
      test_search_both_current_and_archived;
    Alcotest.test_case "session isolation" `Quick test_session_isolation;
    Alcotest.test_case "no results" `Quick test_no_results;
    Alcotest.test_case "limit respected" `Quick test_limit_respected;
    Alcotest.test_case "works without FTS" `Quick test_works_without_fts;
    Alcotest.test_case "works with FTS" `Quick test_works_with_fts;
  ]
