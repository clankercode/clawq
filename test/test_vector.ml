let test_cosine_identical () =
  let v = [| 1.0; 2.0; 3.0 |] in
  let sim = Vector.cosine_similarity v v in
  Alcotest.(check bool) "identical vectors" true (Float.abs (sim -. 1.0) < 1e-9)

let test_cosine_orthogonal () =
  let a = [| 1.0; 0.0 |] in
  let b = [| 0.0; 1.0 |] in
  let sim = Vector.cosine_similarity a b in
  Alcotest.(check bool) "orthogonal vectors" true (Float.abs sim < 1e-9)

let test_cosine_opposite () =
  let a = [| 1.0; 0.0 |] in
  let b = [| -1.0; 0.0 |] in
  let sim = Vector.cosine_similarity a b in
  Alcotest.(check bool) "opposite vectors" true (Float.abs (sim -. (-1.0)) < 1e-9)

let test_cosine_empty () =
  let a = [||] in
  let b = [||] in
  let sim = Vector.cosine_similarity a b in
  Alcotest.(check bool) "empty vectors" true (sim = 0.0)

let test_cosine_mismatch () =
  let a = [| 1.0; 2.0 |] in
  let b = [| 1.0; 2.0; 3.0 |] in
  let sim = Vector.cosine_similarity a b in
  Alcotest.(check bool) "mismatched dimensions" true (sim = 0.0)

let test_cosine_zero () =
  let a = [| 0.0; 0.0 |] in
  let b = [| 1.0; 2.0 |] in
  let sim = Vector.cosine_similarity a b in
  Alcotest.(check bool) "zero vector" true (sim = 0.0)

let test_serialize_roundtrip () =
  let v = [| 1.5; -2.3; 0.0; 42.0; -0.001 |] in
  let blob = Vector.serialize_embedding v in
  let v2 = Vector.deserialize_embedding blob in
  Alcotest.(check int) "same length" (Array.length v) (Array.length v2);
  Array.iteri (fun i x ->
    Alcotest.(check bool) (Printf.sprintf "elem %d" i) true
      (Float.abs (x -. v2.(i)) < 1e-15)
  ) v

let test_serialize_empty () =
  let v = [||] in
  let blob = Vector.serialize_embedding v in
  Alcotest.(check int) "empty blob" 0 (String.length blob);
  let v2 = Vector.deserialize_embedding blob in
  Alcotest.(check int) "empty array" 0 (Array.length v2)

let test_deserialize_bad_size () =
  (* 5 bytes is not a multiple of 8 *)
  let v = Vector.deserialize_embedding "hello" in
  Alcotest.(check int) "bad size returns empty" 0 (Array.length v)

let test_store_and_search () =
  let db = Sqlite3.db_open ":memory:" in
  Vector.init_schema db;
  let emb1 = [| 1.0; 0.0; 0.0 |] in
  let emb2 = [| 0.0; 1.0; 0.0 |] in
  let emb3 = [| 0.9; 0.1; 0.0 |] in
  Vector.store ~db ~session_key:"s1" ~message_id:1L ~content_preview:"hello world" ~embedding:emb1;
  Vector.store ~db ~session_key:"s1" ~message_id:2L ~content_preview:"goodbye" ~embedding:emb2;
  Vector.store ~db ~session_key:"s1" ~message_id:3L ~content_preview:"hi there" ~embedding:emb3;
  let query = [| 1.0; 0.0; 0.0 |] in
  let results = Vector.search ~db ~query_embedding:query ~limit:2 () in
  Alcotest.(check int) "top 2 results" 2 (List.length results);
  let top_content, top_score = List.hd results in
  Alcotest.(check string) "best match" "hello world" top_content;
  Alcotest.(check bool) "perfect match score" true (Float.abs (top_score -. 1.0) < 1e-9);
  ignore (Sqlite3.db_close db)

let test_search_session_filter () =
  let db = Sqlite3.db_open ":memory:" in
  Vector.init_schema db;
  Vector.store ~db ~session_key:"s1" ~message_id:1L ~content_preview:"in s1" ~embedding:[| 1.0; 0.0 |];
  Vector.store ~db ~session_key:"s2" ~message_id:2L ~content_preview:"in s2" ~embedding:[| 0.9; 0.1 |];
  let query = [| 1.0; 0.0 |] in
  let results = Vector.search ~db ~query_embedding:query ~session_key:"s2" ~limit:10 () in
  Alcotest.(check int) "only s2 results" 1 (List.length results);
  let content, _ = List.hd results in
  Alcotest.(check string) "s2 content" "in s2" content;
  ignore (Sqlite3.db_close db)

let test_merge_keyword_only () =
  let merged = Vector.merge_results
      ~keyword_results:["a"; "b"; "c"]
      ~vector_results:[]
      ~keyword_weight:100 ~vector_weight:0 in
  Alcotest.(check int) "3 results" 3 (List.length merged);
  Alcotest.(check string) "first is a" "a" (List.hd merged)

let test_merge_vector_only () =
  let merged = Vector.merge_results
      ~keyword_results:[]
      ~vector_results:[("x", 0.9); ("y", 0.5)]
      ~keyword_weight:0 ~vector_weight:100 in
  Alcotest.(check int) "2 results" 2 (List.length merged);
  Alcotest.(check string) "first is x" "x" (List.hd merged)

let test_merge_hybrid () =
  (* "a" is top keyword result (kw_score=1.0), "b" is top vector result (vec_score=1.0) *)
  let merged = Vector.merge_results
      ~keyword_results:["a"; "b"]
      ~vector_results:[("b", 0.95); ("a", 0.3)]
      ~keyword_weight:50 ~vector_weight:50 in
  Alcotest.(check int) "2 results" 2 (List.length merged);
  (* "a": kw=1.0, vec_norm=(0.3-0.3)/(0.95-0.3)=0.0 -> 0.5*1.0 + 0.5*0.0 = 0.5 *)
  (* "b": kw=0.9, vec_norm=(0.95-0.3)/(0.95-0.3)=1.0 -> 0.5*0.9 + 0.5*1.0 = 0.95 *)
  Alcotest.(check string) "b wins with vector boost" "b" (List.hd merged)

let test_merge_dedup () =
  let merged = Vector.merge_results
      ~keyword_results:["same"; "other"]
      ~vector_results:[("same", 0.8)]
      ~keyword_weight:50 ~vector_weight:50 in
  Alcotest.(check int) "deduplicated" 2 (List.length merged)

let test_init_schema () =
  let db = Sqlite3.db_open ":memory:" in
  Vector.init_schema db;
  (* Should not fail on double init *)
  Vector.init_schema db;
  Alcotest.(check bool) "schema init idempotent" true true;
  ignore (Sqlite3.db_close db)

let suite =
  [
    Alcotest.test_case "cosine identical" `Quick test_cosine_identical;
    Alcotest.test_case "cosine orthogonal" `Quick test_cosine_orthogonal;
    Alcotest.test_case "cosine opposite" `Quick test_cosine_opposite;
    Alcotest.test_case "cosine empty" `Quick test_cosine_empty;
    Alcotest.test_case "cosine mismatch" `Quick test_cosine_mismatch;
    Alcotest.test_case "cosine zero vector" `Quick test_cosine_zero;
    Alcotest.test_case "serialize roundtrip" `Quick test_serialize_roundtrip;
    Alcotest.test_case "serialize empty" `Quick test_serialize_empty;
    Alcotest.test_case "deserialize bad size" `Quick test_deserialize_bad_size;
    Alcotest.test_case "store and search" `Quick test_store_and_search;
    Alcotest.test_case "search session filter" `Quick test_search_session_filter;
    Alcotest.test_case "merge keyword only" `Quick test_merge_keyword_only;
    Alcotest.test_case "merge vector only" `Quick test_merge_vector_only;
    Alcotest.test_case "merge hybrid" `Quick test_merge_hybrid;
    Alcotest.test_case "merge dedup" `Quick test_merge_dedup;
    Alcotest.test_case "init schema idempotent" `Quick test_init_schema;
  ]
