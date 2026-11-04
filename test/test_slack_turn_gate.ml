open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads
module F = Test_slack_event_fixtures

let test_slackbot_reply () =
  with_ingress (fun _ calls _ seen _ run ->
      let* _ =
        run
          (update
             (message ~user:"USLACKBOT" ~thread:"100.000001" ~ts:"100.000009"
                "AI huddle notes are ready")
             [ ("files", arr [ F.canvas_file () ]) ])
      in
      check_bool "notes-ready reply: no Slack call" true (!calls = []);
      check_bool "notes-ready reply: no model turn" true (!seen = []);
      Lwt.return_unit)

let test_slackbot_mention () =
  with_ingress (fun _ calls _ seen _ run ->
      let* _ =
        run (message ~kind:"app_mention" ~user:"USLACKBOT" "<@UBOT> hello")
      in
      check_bool "Slackbot mention: no Slack call" true (!calls = []);
      check_bool "Slackbot mention: no model turn" true (!seen = []);
      Lwt.return_unit)

let test_human_turn () =
  with_ingress (fun _ _ _ seen _ run ->
      let* _ = run (message "please help") in
      check_bool "human message reaches the model" true (!seen <> []);
      Lwt.return_unit)

let suite =
  [
    ("slackbot thread reply starts no turn", `Quick, test_slackbot_reply);
    ("slackbot mention starts no turn", `Quick, test_slackbot_mention);
    ("human message still starts a turn", `Quick, test_human_turn);
  ]
