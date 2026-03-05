let time f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  let elapsed = t1 -. t0 in
  (result, elapsed)

let test_memory_store_1000 () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  let _, elapsed =
    time (fun () ->
        for i = 0 to 999 do
          Memory.store_message ~db ~session_key:"bench"
            (Provider.make_message ~role:"user"
               ~content:("msg " ^ string_of_int i))
        done)
  in
  Printf.printf "[perf] memory_store_1000: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0);
  ignore (Sqlite3.db_close db)

let test_memory_load_1000 () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  for i = 0 to 999 do
    Memory.store_message ~db ~session_key:"bench"
      (Provider.make_message ~role:"user"
         ~content:("msg " ^ string_of_int i))
  done;
  let _, elapsed =
    time (fun () ->
        let msgs = Memory.load_history ~db ~session_key:"bench" in
        Alcotest.(check bool) "loaded 1000" true (List.length msgs = 1000))
  in
  Printf.printf "[perf] memory_load_1000: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0);
  ignore (Sqlite3.db_close db)

let test_fts_search_100 () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  for i = 0 to 99 do
    Memory.store_message ~db ~session_key:"bench"
      (Provider.make_message ~role:"user"
         ~content:("msg " ^ string_of_int i))
  done;
  let _, elapsed =
    time (fun () ->
        for _ = 0 to 9 do
          ignore (Memory.search ~db ~query:"msg" ~limit:10 ())
        done)
  in
  Printf.printf "[perf] fts_search_100: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0);
  ignore (Sqlite3.db_close db)

let sample_config_json : Yojson.Safe.t =
  `Assoc
    [
      ("default_temperature", `Float 0.7);
      ("default_provider", `String "openai");
      ( "providers",
        `Assoc
          [
            ( "openai",
              `Assoc
                [
                  ("api_key", `String "sk-test-key");
                  ("default_model", `String "gpt-4");
                ] );
          ] );
      ( "agent_defaults",
        `Assoc
          [
            ("primary_model", `String "gpt-4");
            ("system_prompt", `String "You are helpful.");
            ("max_tool_iterations", `Int 10);
          ] );
      ( "memory",
        `Assoc
          [
            ("backend", `String "sqlite");
            ("search_enabled", `Bool true);
            ("db_path", `String ":memory:");
          ] );
    ]

let test_config_parse () =
  let _, elapsed =
    time (fun () ->
        for _ = 1 to 1000 do
          ignore (Config_loader.parse_config sample_config_json)
        done)
  in
  Printf.printf "[perf] config_parse_1000: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0)

let make_random_vector dim =
  Array.init dim (fun _ -> Random.float 2.0 -. 1.0)

let test_vector_cosine_10000 () =
  Random.self_init ();
  let a = make_random_vector 1536 in
  let b = make_random_vector 1536 in
  let _, elapsed =
    time (fun () ->
        for _ = 1 to 10000 do
          ignore (Vector.cosine_similarity a b)
        done)
  in
  Printf.printf "[perf] vector_cosine_10000: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0)

let test_vector_serialize_roundtrip () =
  Random.self_init ();
  let embeddings = Array.init 1000 (fun _ -> make_random_vector 1536) in
  let _, elapsed =
    time (fun () ->
        Array.iter
          (fun emb ->
            let blob = Vector.serialize_embedding emb in
            let _restored = Vector.deserialize_embedding blob in
            ())
          embeddings)
  in
  Printf.printf "[perf] vector_serialize_roundtrip_1000: %.4f s\n%!" elapsed;
  Alcotest.(check bool) "completes in time" true (elapsed < 5.0)

let suite =
  [
    Alcotest.test_case "memory_store_1000" `Quick test_memory_store_1000;
    Alcotest.test_case "memory_load_1000" `Quick test_memory_load_1000;
    Alcotest.test_case "fts_search_100" `Quick test_fts_search_100;
    Alcotest.test_case "config_parse" `Quick test_config_parse;
    Alcotest.test_case "vector_cosine_10000" `Quick test_vector_cosine_10000;
    Alcotest.test_case "vector_serialize_roundtrip" `Quick
      test_vector_serialize_roundtrip;
  ]
