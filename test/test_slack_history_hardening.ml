open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads
module F = Test_slack_event_fixtures
module G = Slack_relay_guard

let route =
  {
    Slack_thread_route.team = "T123";
    app = "A123";
    channel = "C123";
    root = "100.000001";
  }

let render ?(untrusted = fun _ -> false) messages =
  Slack_thread_api.render_history ~route ~bot:"UBOT" ~current:"100.9" ~untrusted
    {
      Slack_thread_api.messages;
      overflow = false;
      limited = false;
      next_cursor = "";
      has_more = false;
    }

let own_reply =
  update
    (message ~ts:"100.2" ~user:"UBOT" "own response")
    [ ("bot_id", str "BOWN") ]

let untrusted_role = "\"role\":\"untrusted_source\""
let assistant_role = "\"role\":\"assistant\""
let json = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let seen_text seen =
  List.concat !seen
  |> List.map (fun (m : Provider.message) -> m.content)
  |> String.concat "\n"

let test_allowlist () =
  let rendered =
    render
      [
        update
          (message ~ts:"100.000001" "see file")
          [ ("files", arr [ F.md_file (); F.transcript_file (); str "junk" ]) ];
      ]
  in
  List.iter
    (fun kept -> check_bool ("keeps " ^ kept) true (contains rendered kept))
    [
      "F0MD";
      "notes.md";
      "text/markdown";
      "F0TRANSCRIPT";
      "application/vnd.slack-huddle-transcript";
    ];
  List.iter
    (fun dropped ->
      check_bool ("drops " ^ dropped) false (contains rendered dropped))
    [
      "SECRET";
      "title_blocks";
      "files-pri";
      "preview";
      "huddle_transcription";
      "junk";
    ]

(* Pins the exact allowlist and its reuse of [Slack_event_log.scalars]: only
   scalar values of the eight keys survive. *)
let test_exact_allowlist () =
  Alcotest.(check (list string))
    "history file keys"
    [ "id"; "name"; "title"; "mimetype"; "filetype"; "size"; "user"; "created" ]
    Slack_thread_api.history_file_keys;
  Alcotest.(check (option json))
    "markdown file keeps exactly the eight keys"
    (Some
       (obj
          [
            ("id", str "F0MD");
            ("name", str "notes.md");
            ("title", str "notes");
            ("mimetype", str "text/markdown");
            ("filetype", str "markdown");
            ("size", `Int 10);
            ("user", str "UBOT");
            ("created", `Int 1700000000);
          ]))
    (Slack_thread_api.project_file (F.md_file ()));
  Alcotest.(check (option json))
    "transcript file keeps only its allowlisted keys"
    (Some
       (obj
          [
            ("id", str "F0TRANSCRIPT");
            ("mimetype", str "application/vnd.slack-huddle-transcript");
            ("filetype", str "huddle_transcript");
            ("user", str "U0STARTER");
          ]))
    (Slack_thread_api.project_file (F.transcript_file ()));
  Alcotest.(check (option json))
    "non-scalar values of allowlisted keys are dropped"
    (Some (obj [ ("id", str "F1") ]))
    (Slack_thread_api.project_file
       (obj
          [
            ("id", str "F1");
            ("name", arr [ str "SECRET" ]);
            ("title", obj [ ("text", str "SECRET") ]);
            ("size", `Null);
          ]))

(* Spec 6.7: a shared or forwarded message's attachment carries full file
   objects; they get the same allowlist as top-level files. *)
let test_attachment_files () =
  let rendered =
    render
      [
        update
          (message ~ts:"100.000001" "shared")
          [
            ( "attachments",
              arr
                [
                  obj
                    [
                      ("is_share", `Bool true);
                      ("text", str "shared message text");
                      ("files", arr [ F.md_file () ]);
                      ( "message_blocks",
                        arr
                          [
                            obj
                              [
                                ( "message",
                                  obj
                                    [
                                      ("files", arr [ F.transcript_file () ]);
                                      ("file", F.md_file ~id:"F0NESTED" ());
                                    ] );
                              ];
                          ] );
                    ];
                ] );
          ];
      ]
  in
  List.iter
    (fun kept -> check_bool ("keeps " ^ kept) true (contains rendered kept))
    [
      "F0MD";
      "notes.md";
      "F0TRANSCRIPT";
      "F0NESTED";
      "is_share";
      "shared message text";
    ];
  (* Bare words: the metadata is JSON quoted inside the history's JSON. *)
  List.iter
    (fun dropped ->
      check_bool ("drops " ^ dropped) false (contains rendered dropped))
    [
      "SECRET";
      "preview";
      "huddle_transcription";
      "title_blocks";
      "files-pri";
      "lines";
    ]

let test_empty_projection () =
  check_bool "no metadata suffix" false
    (contains
       (render
          [
            update (message ~ts:"100.3" "hi") [ ("files", arr [ str "junk" ]) ];
          ])
       "Attached source metadata")

let test_own_relay () =
  let relay = render [ F.own_relay ~channel:"C123" ~ts:"100.000001" () ] in
  check_bool "structural relay is untrusted" true
    (contains relay untrusted_role);
  check_bool "structural relay is never assistant" false
    (contains relay assistant_role);
  check_bool "own reply stays assistant" true
    (contains (render [ own_reply ]) assistant_role);
  let flagged =
    render ~untrusted:(fun m -> get "ts" m = str "100.2") [ own_reply ]
  in
  check_bool "registry hit is untrusted" true (contains flagged untrusted_role);
  check_bool "registry hit is not assistant" false
    (contains flagged assistant_role);
  let recorded =
    {
      G.no_relays with
      recorded_relay = (fun ~channel ~ts -> channel = "C123" && ts = "100.2");
    }
  in
  check_bool "recorded relay ts" true
    (G.untrusted_in_history ~registry:recorded ~channel:"C123" own_reply);
  check_bool "no recorded relays" false
    (G.untrusted_in_history ~registry:G.no_relays ~channel:"C123" own_reply)

let test_mentioned_relay () =
  with_ingress (fun _ _ handler seen _ run ->
      Test_slack_relay_guard.with_replies handler
        [ F.own_relay ~channel:"C123" ~ts:"100.000001" () ];
      let* _ =
        run (message ~thread:"100.000001" ~ts:"100.000004" "<@UBOT> summarise")
      in
      let text = seen_text seen in
      check_bool "own relay quoted as untrusted" true
        (contains text
           "\"role\":\"untrusted_source\",\"text\":\"<@U0CLAUDE> Huddle notes");
      check_bool "file previews never reach the model" false
        (contains text "SECRET");
      Lwt.return_unit)

(* Ingress hands render_history the registry check for the event's channel. *)
let history_through_ingress ?registry () =
  let text = ref "" in
  with_ingress (fun _ _ handler seen _ run ->
      Test_slack_relay_guard.with_replies handler
        [
          message "root context";
          update
            (message ~ts:"100.000002" ~user:"UBOT" "own earlier reply")
            [ ("bot_id", str "BOWN") ];
        ];
      let reply () =
        run (message ~thread:"100.000001" ~ts:"100.000004" "what now")
      in
      let* _ =
        match registry with
        | None -> reply ()
        | Some registry -> Test_slack_relay_guard.with_registry registry reply
      in
      text := seen_text seen;
      Lwt.return_unit);
  !text

let test_ingress_registry_label () =
  let own role = "\"author\":\"UBOT\",\"role\":\"" ^ role ^ "\"" in
  let control = history_through_ingress () in
  check_bool "control: own reply is assistant" true
    (contains control (own "assistant"));
  let flagged =
    history_through_ingress
      ~registry:
        {
          G.no_relays with
          recorded_relay =
            (fun ~channel ~ts -> channel = "C123" && ts = "100.000002");
        }
      ()
  in
  check_bool "recorded relay in the event's channel is untrusted" true
    (contains flagged (own "untrusted_source"));
  check_bool "recorded relay is not assistant" false
    (contains flagged (own "assistant"))

let suite =
  [
    ("history file-key allowlist", `Quick, test_allowlist);
    ("exact file-key allowlist", `Quick, test_exact_allowlist);
    ("attachment files use the allowlist", `Quick, test_attachment_files);
    ("empty file projection omitted", `Quick, test_empty_projection);
    ("own relay labelled untrusted", `Quick, test_own_relay);
    ("mentioned relay thread context hardened", `Quick, test_mentioned_relay);
    ( "ingress labels recorded relays from the registry",
      `Quick,
      test_ingress_registry_label );
  ]
