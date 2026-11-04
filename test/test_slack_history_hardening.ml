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
      let text =
        List.concat !seen
        |> List.map (fun (m : Provider.message) -> m.content)
        |> String.concat "\n"
      in
      check_bool "own relay quoted as untrusted" true
        (contains text
           "\"role\":\"untrusted_source\",\"text\":\"<@U0CLAUDE> Huddle notes");
      check_bool "file previews never reach the model" false
        (contains text "SECRET");
      Lwt.return_unit)

let suite =
  [
    ("history file-key allowlist", `Quick, test_allowlist);
    ("empty file projection omitted", `Quick, test_empty_projection);
    ("own relay labelled untrusted", `Quick, test_own_relay);
    ("mentioned relay thread context hardened", `Quick, test_mentioned_relay);
  ]
