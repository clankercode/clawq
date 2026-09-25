let test_join_bodies () =
  let joined = Github_notify_batch.join_bodies [ "one"; "two"; "three" ] in
  Alcotest.(check string) "joined" "one\n<hr>\ntwo\n<hr>\nthree" joined

let test_should_flush_waits_for_window () =
  Alcotest.(check bool)
    "too fresh" false
    (Github_notify_batch.should_flush ~now:100. ~last_sent:0. ~oldest:90.
       ~window_s:20.);
  Alcotest.(check bool)
    "window elapsed" true
    (Github_notify_batch.should_flush ~now:120. ~last_sent:0. ~oldest:90.
       ~window_s:20.);
  Alcotest.(check bool)
    "spacing after send" false
    (Github_notify_batch.should_flush ~now:130. ~last_sent:120. ~oldest:90.
       ~window_s:20.)

let test_take_oldest_caps_at_ten () =
  Test_helpers.with_memory_db (fun db ->
      for i = 1 to 12 do
        Github_notify_batch.enqueue ~db ~room_id:"room-a"
          ~body:(Printf.sprintf "msg-%d" i)
      done;
      Github_notify_batch.enqueue ~db ~room_id:"room-b" ~body:"other";
      let batch =
        Github_notify_batch.take_oldest ~db ~room_id:"room-a" ~limit:10
      in
      Alcotest.(check int) "batch size" 10 (List.length batch);
      Alcotest.(check string) "oldest first" "msg-1" (snd (List.hd batch));
      Alcotest.(check string) "tenth" "msg-10" (snd (List.nth batch 9));
      let joined = Github_notify_batch.join_bodies (List.map snd batch) in
      Alcotest.(check bool)
        "separator present" true
        (String.length joined > 0 && String.contains joined 'h'))

let test_trim_keeps_newest () =
  Test_helpers.with_memory_db (fun db ->
      for i = 1 to 5 do
        Github_notify_batch.enqueue ~db ~room_id:"room" ~body:(string_of_int i)
      done;
      let trimmed =
        match Github_notify_batch.trim ~db ~keep:2 with
        | Ok r -> r
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check int) "deleted" 3 trimmed.deleted;
      Alcotest.(check int) "remaining" 2 trimmed.remaining;
      let left =
        Github_notify_batch.take_oldest ~db ~room_id:"room" ~limit:10
        |> List.map snd
      in
      Alcotest.(check (list string)) "newest two" [ "4"; "5" ] left;
      let cleared =
        match Github_notify_batch.trim ~db ~keep:0 with
        | Ok r -> r
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check int) "cleared remaining" 0 cleared.remaining)

let test_parse_keep_defaults_to_55 () =
  Alcotest.(check int)
    "default" 55
    (match Github_webhooks_cli.parse_keep [] with
    | Ok n -> n
    | Error e -> Alcotest.fail e);
  Alcotest.(check int)
    "bare zero" 0
    (match Github_webhooks_cli.parse_keep [ "0" ] with
    | Ok n -> n
    | Error e -> Alcotest.fail e);
  Alcotest.(check int)
    "flag" 12
    (match Github_webhooks_cli.parse_keep [ "--keep"; "12" ] with
    | Ok n -> n
    | Error e -> Alcotest.fail e)

let test_clear_queue_leaves_other_rows () =
  Test_helpers.with_memory_db (fun db ->
      for i = 1 to 4 do
        Github_notify_batch.enqueue ~db ~room_id:"room" ~body:(string_of_int i)
      done;
      let report =
        match Github_webhooks_cli.clear ~db ~keep:2 () with
        | Ok r -> r
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check int) "deleted" 2 report.deleted;
      Alcotest.(check int) "remaining" 2 report.remaining;
      let left =
        Github_notify_batch.take_oldest ~db ~room_id:"room" ~limit:10
        |> List.map snd
      in
      Alcotest.(check (list string)) "newest queued texts" [ "3"; "4" ] left)

let suite =
  [
    Alcotest.test_case "join bodies with hr" `Quick test_join_bodies;
    Alcotest.test_case "flush waits for the window" `Quick
      test_should_flush_waits_for_window;
    Alcotest.test_case "take oldest caps at ten" `Quick
      test_take_oldest_caps_at_ten;
    Alcotest.test_case "trim keeps newest rows" `Quick test_trim_keeps_newest;
    Alcotest.test_case "keep defaults to 55" `Quick
      test_parse_keep_defaults_to_55;
    Alcotest.test_case "clear-queue keeps newest unsent texts" `Quick
      test_clear_queue_leaves_other_rows;
  ]
