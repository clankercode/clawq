module T = Slack_huddle_transcript

let line ?line_id ?(start = `Int 61234) ?(user = `String "U123")
    ?(contents = `String "hello") () =
  `Assoc
    ([ ("start_time_ms", start); ("user_id", user); ("contents", contents) ]
    @ match line_id with None -> [] | Some id -> [ ("line_id", id) ])

let response lines =
  `Assoc
    [
      ("ok", `Bool true);
      ("file", `Assoc [ ("huddle_transcription", `Assoc [ ("lines", lines) ]) ]);
    ]

let decode json =
  match T.of_file_info json with
  | Some lines -> lines
  | None -> Alcotest.fail "expected usable transcription"

let render ?(date_start = 1700000000) speaker_name lines =
  Lwt_main.run (T.render ~speaker_name ~date_start lines)

let alice _ = Lwt.return "Alice"

let test_relative_offsets () =
  let lines = decode (response (`List [ line () ])) in
  Alcotest.(check string)
    "relative timestamp" "[01:01] Alice: hello\n" (render alice lines);
  Alcotest.(check string)
    "room date does not affect offset" "[01:01] Alice: hello\n"
    (render ~date_start:0 alice lines);
  let large = decode (response (`List [ line ~start:(`Int 1700000000) () ])) in
  Alcotest.(check string)
    "epoch-sized offsets are still relative" "[28333:20] Alice: hello\n"
    (render alice large)

let test_stable_ids () =
  let lines =
    decode
      (response
         (`List [ line (); line ~line_id:(`String "source-id") (); line () ]))
  in
  Alcotest.(check (list string))
    "stable source indexes"
    [ "line-0"; "source-id"; "line-2" ]
    (List.map (fun (l : T.line) -> l.line_id) lines)

let test_unavailable () =
  let valid = line () in
  let without_contents =
    `Assoc [ ("start_time_ms", `Int 0); ("user_id", `String "U123") ]
  in
  let malformed_lines =
    [
      ("negative offset", line ~start:(`Int (-1)) ());
      ("nonnumeric offset", line ~start:(`String "no") ());
      ("numeric string offset", line ~start:(`String "61234") ());
      ("fractional offset", line ~start:(`Float 1.5) ());
      ( "missing offset",
        `Assoc [ ("user_id", `String "U123"); ("contents", `String "hello") ] );
      ("missing contents", without_contents);
      ("nonstring contents", line ~contents:`Null ());
      ("invalid speaker", line ~user:(`String "C123") ());
      ("malformed speaker", line ~user:(`String "U12_3") ());
      ( "missing speaker",
        `Assoc [ ("start_time_ms", `Int 0); ("contents", `String "hello") ] );
      ("malformed line id", line ~line_id:(`Int 1) ());
      ("nonobject line", `Null);
    ]
  in
  let unavailable =
    [
      ("missing file", `Assoc []);
      ("null file", `Assoc [ ("file", `Null) ]);
      ("missing transcription", `Assoc [ ("file", `Assoc []) ]);
      ( "empty transcription",
        `Assoc [ ("file", `Assoc [ ("huddle_transcription", `Assoc []) ]) ] );
      ( "missing lines",
        `Assoc
          [
            ( "file",
              `Assoc
                [
                  ( "huddle_transcription",
                    `Assoc [ ("blocks", `List []); ("text", `String "hello") ]
                  );
                ] );
          ] );
      ("empty lines", response (`List []));
      ("nonlist lines", response (`String "hello"));
      ( "wrong nesting",
        `Assoc
          [ ("huddle_transcription", `Assoc [ ("lines", `List [ valid ]) ]) ] );
    ]
    @ List.map
        (fun (name, malformed) -> (name, response (`List [ valid; malformed ])))
        malformed_lines
  in
  List.iter
    (fun (name, json) ->
      Alcotest.(check bool) name true (Option.is_none (T.of_file_info json)))
    unavailable

let test_large_unknown () =
  let text = String.make 200000 'x' in
  let lines = decode (response (`List [ line ~contents:(`String text) () ])) in
  let failed _ = Lwt.fail (Failure "lookup unavailable") in
  Alcotest.(check string)
    "lossless transcript despite lookup failure"
    ("[01:01] Unknown speaker (U123): " ^ text ^ "\n")
    (render failed lines);
  List.iter
    (fun lookup ->
      Alcotest.(check string)
        "fallback for synchronous failure or empty name"
        ("[01:01] Unknown speaker (U123): " ^ text ^ "\n")
        (render lookup lines))
    [
      (fun _ -> failwith "lookup failed synchronously");
      (fun _ -> Lwt.return "");
    ]

let test_content_and_speakers () =
  let text = "<@U456> <!channel> [link](https://example.invalid)\nα\000\r\n" in
  let lines =
    decode
      (response
         (`List
            [
              line ~start:(`Int 3600001) ~contents:(`String text) ();
              line ~start:(`Int 0) ~user:(`String "W123") ~contents:(`String "")
                ();
            ]))
  in
  let seen = ref [] in
  let lookup id =
    seen := id :: !seen;
    Lwt.return (if id = "U123" then "Alice" else "Bob")
  in
  Alcotest.(check string)
    "content bytes, line order and unwrapped minutes"
    ("[60:00] Alice: " ^ text ^ "\n[00:00] Bob: \n")
    (render lookup lines);
  Alcotest.(check (list string))
    "only line speakers resolved, never text mentions" [ "U123"; "W123" ]
    (List.rev !seen)

let test_no_global_cache () =
  let lines = decode (response (`List [ line () ])) in
  ignore (render alice lines);
  Alcotest.(check string)
    "lookup belongs to caller lifetime" "[01:01] Renamed: hello\n"
    (render (fun _ -> Lwt.return "Renamed") lines)

let suite =
  [
    Alcotest.test_case "relative offsets never subtract room date_start" `Quick
      test_relative_offsets;
    Alcotest.test_case "missing optional line id is stable" `Quick
      test_stable_ids;
    Alcotest.test_case "absent empty and malformed transcription is None" `Quick
      test_unavailable;
    Alcotest.test_case "large transcript and unknown speaker" `Quick
      test_large_unknown;
    Alcotest.test_case
      "content bytes and order survive without mention resolution" `Quick
      test_content_and_speakers;
    Alcotest.test_case "speaker names have no global cache" `Quick
      test_no_global_cache;
  ]
