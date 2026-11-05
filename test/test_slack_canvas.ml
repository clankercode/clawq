open Lwt.Syntax
module C = Slack_canvas

let check = Alcotest.(check bool)
let run = Lwt_main.run
let ok fields = Ok (`Assoc (("ok", `Bool true) :: fields))

let test_primitives () =
  let calls = ref [] in
  let call method_ params =
    calls := !calls @ [ (method_, `Assoc params) ];
    Lwt.return
      (match method_ with
      | "canvases.create" -> ok [ ("canvas_id", `String "F123") ]
      | "files.info" ->
          ok
            [
              ( "file",
                `Assoc
                  [
                    ("id", `String "F123");
                    ("permalink", `String "https://example.slack.com/docs/F123");
                  ] );
            ]
      | _ -> ok [])
  in
  run
    (let* id = C.create ~call ~title:"Title" ~markdown:"Body" in
     check "created" true (id = Ok "F123");
     let* link = C.permalink ~call ~canvas_id:"F123" in
     check "authoritative permalink" true
       (link = Ok "https://example.slack.com/docs/F123");
     let* grant = C.grant_read ~call ~canvas_id:"F123" ~channel_id:"C123" in
     check "grant" true (grant = Ok ());
     let+ _ = C.delete ~call ~canvas_id:"F123" in
     let get k j = Yojson.Safe.Util.member k j in
     check "standalone" true (get "channel_id" (snd (List.hd !calls)) = `Null);
     check "one read channel" true
       (get "access_level" (snd (List.nth !calls 2)) = `String "read"
       && get "channel_ids" (snd (List.nth !calls 2)) = `List [ `String "C123" ]
       && get "user_ids" (snd (List.nth !calls 2)) = `Null))

let test_untrusted_link () =
  List.iter
    (fun link ->
      let call _ _ =
        Lwt.return
          (ok
             [
               ( "file",
                 `Assoc [ ("id", `String "F123"); ("permalink", `String link) ]
               );
             ])
      in
      check "unsafe link rejected" true
        (match run (C.permalink ~call ~canvas_id:"F123") with
        | Error _ -> true
        | _ -> false))
    [
      "http://example.slack.com/a";
      "https://evil.example/x";
      "https://example.slack.com/a>|<!here>";
      "";
    ]

let test_selectors () =
  let d : C.destination =
    {
      session_key = "slack:C123";
      channel_id = "C123";
      thread_ts = Some "100.000001";
      requester = "U123";
    }
  in
  List.iter
    (fun (method_, params) ->
      check "unsafe selectors denied" false (C.selectors d method_ params))
    [
      ( "canvases.access.set",
        [
          ("canvas_id", `String "F123");
          ("access_level", `String "read");
          ("access_level", `String "write");
          ("channel_ids", `List [ `String "C123" ]);
        ] );
      ( "chat.postMessage",
        [
          ("channel", `String "C123");
          ("thread_ts", `String "100.000001");
          ("reply_broadcast", `Bool true);
        ] );
      ( "chat.postMessage",
        [ ("channel", `String "C123"); ("thread_ts", `String "other") ] );
      ( "canvases.create",
        [ ("title", `String "title"); ("channel_id", `String "C123") ] );
      ( "chat.postMessage",
        [
          ("channel", `String "C123");
          ("thread_ts", `String "100.000001");
          ("blocks", `List []);
        ] );
    ]

let suite =
  [
    ("checked canvas primitives", `Quick, test_primitives);
    ("untrusted permalink is rejected", `Quick, test_untrusted_link);
    ("duplicate and unknown selectors are rejected", `Quick, test_selectors);
  ]
