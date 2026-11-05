(* B843: in channels clawq replies only when mentioned, or in a thread right
   after its own reply; [reply_mode = all] keeps all-message admission. *)
open Lwt.Syntax
open Test_slack_huddle_fixtures
module C = Test_slack_standing_order_config
module F = Test_slack_standing_order_fixtures
module Pause = Test_slack_standing_order_pause
module Th = Test_slack_threads

let check_bool = Alcotest.(check bool)
let message = Th.message
let set_reply_mode = Th.set_reply_mode

let with_reply_gate ?(mode = "mention") ?(channel = "C123") ?posts f =
  Th.with_ingress (fun base calls handler seen _ run ->
      set_reply_mode base mode;
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "conversations.info" && channel = "D123" then
             ok
               [
                 ( "channel",
                   obj
                     [
                       ("id", str channel);
                       ("is_im", `Bool true);
                       ("user", str "U123");
                     ] );
               ]
           else if m = "conversations.replies" then
             ok
               [
                 ( "messages",
                   arr (Option.value posts ~default:[ message "root" ]) );
                 ("has_more", `Bool false);
               ]
           else old m p);
      let run ?id e = run ?id (update e [ ("channel", str channel) ]) in
      f base calls seen run)

(* Reads only: no post, reaction, status or stream. *)
let check_mention_silent calls seen =
  check_bool "no provider call" true (!seen = []);
  check_bool "no Slack write or reaction" true
    (List.for_all
       (fun (m, _) ->
         List.mem m
           [
             "auth.test";
             "users.info";
             "conversations.info";
             "conversations.members";
             "conversations.replies";
           ])
       !calls)

let check_turn seen = check_bool "provider called" true (!seen <> [])

let test_top_silent () =
  with_reply_gate (fun _ calls seen run ->
      let* ack, logs =
        Test_slack_standing_order_support.with_log_capture (fun () ->
            run (message "human conversation"))
      in
      Alcotest.(check string) "acknowledged" "ok" ack;
      check_mention_silent calls seen;
      check_bool "info reason" true
        (List.mem
           ( Logs.Info,
             "Slack C123/100.000001: no turn without an explicit mention \
              (reply_mode=mention)" )
           logs);
      Lwt.return_unit)

let test_top_mention () =
  with_reply_gate (fun _ _ seen run ->
      let* _ = run (message "<@UBOT> help") in
      check_turn seen;
      Lwt.return_unit)

let huddle_root = Test_slack_huddle_fixtures.root
let own_reply ts = message ~ts ~user:"UBOT" "clawq answer"

let test_huddle_silent () =
  with_reply_gate ~posts:[ huddle_root ] (fun _ calls seen run ->
      let* _ =
        run
          (message ~thread:"100.000001" ~ts:"1100.000001" "human conversation")
      in
      check_mention_silent calls seen;
      Lwt.return_unit)

(* The reply at 1100.000001 follows [posts] in the huddle thread. *)
let test_followup ~text ~posts ~turn () =
  with_reply_gate ~posts (fun _ calls seen run ->
      let* _ = run (message ~thread:"100.000001" ~ts:"1100.000001" text) in
      if turn then check_turn seen else check_mention_silent calls seen;
      Lwt.return_unit)

let after_own ?(text = "thanks, more?") prior ~turn =
  test_followup ~text ~posts:[ huddle_root; own_reply prior ] ~turn

let test_dm () =
  with_reply_gate ~channel:"D123" (fun _ _ seen run ->
      let* _ = run (message "help") in
      check_turn seen;
      Lwt.return_unit)

let test_all () =
  with_reply_gate ~mode:"all" ~posts:[ huddle_root ] (fun _ _ seen run ->
      let* _ = run (message "top") in
      let* _ =
        run ~id:"reply" (message ~thread:"100.000001" ~ts:"1100.000001" "reply")
      in
      Alcotest.(check int) "both events run" 2 (List.length !seen);
      Lwt.return_unit)

let test_pause () =
  F.with_orders ~config:F.mention_mode (fun env ->
      let o = F.seed env () in
      let* _ = F.say env ~thread:F.thread_root "pause SO-1" in
      Pause.check_state env o Slack_standing_order_types.Paused;
      check_bool "pause receipt" true
        (Pause.has_text env (Pause.paused_receipt ~by:"U123"));
      check_bool "no provider call" true (!(env.seen) = []);
      check_bool "pause precedes history" false
        (Pause.called env "conversations.replies");
      Lwt.return_unit)

(* The connector's startup copy stays [mention]; only the live config moves. *)
let test_reload () =
  with_reply_gate (fun base calls seen _ ->
      let startup =
        Option.get (Session.get_config base.manager).channels.slack
      in
      let run ~id event =
        Slack.handle_event ~config:startup ~session_manager:base.manager
          (Yojson.Safe.to_string (Th.callback ~id event))
      in
      let* _ = run ~id:"startup" (message "silent") in
      check_mention_silent calls seen;
      set_reply_mode base "all";
      let* _ = run ~id:"all" (message ~ts:"1100.000001" "now answer") in
      Alcotest.(check int) "live all" 1 (List.length !seen);
      set_reply_mode base "mention";
      calls := [];
      seen := [];
      let* _ = run ~id:"mention" (message ~ts:"1101.000001" "silent again") in
      check_mention_silent calls seen;
      Lwt.return_unit)

let test_command () =
  with_reply_gate (fun base calls seen run ->
      let* _ = run (message "/help") in
      check_mention_silent calls seen;
      check_bool "no session" true (Hashtbl.length base.manager.sessions = 1);
      Lwt.return_unit)

(* A [!] interrupt from the owner of the thread's active run is addressed to
   clawq: its instruction runs after the interruption, as in [all] mode. *)
let test_bang () =
  Test_slack_fix1_fixtures.with_recording (fun base _ _ _ _ provider run ->
      set_reply_mode base "mention";
      let gate, release = Lwt.task () in
      let inputs = ref [] in
      (provider :=
         fun ~model ~messages ->
           inputs := !inputs @ [ messages ];
           if List.length !inputs = 1 then gate
           else Lwt.return (Test_slack_fix1_fixtures.text model "new direction"));
      let work = run (message "<@UBOT> held work") in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        run ~id:"control"
          (message ~thread:"100.000001" ~ts:"100.000003" "!change direction")
      in
      let interrupt =
        Session.interrupt_check_if_present base.manager
          ~key:Test_slack_fix1_fixtures.key ()
      in
      Th.release_text release "gpt-test";
      let* _ = work in
      Alcotest.(check (option string))
        "bang interrupts the held turn" (Some "change direction") interrupt;
      Alcotest.(check int)
        "bang instruction runs after the interruption" 2 (List.length !inputs);
      Lwt.return_unit)

let mode cfg =
  Config_set.json_get
    [ "channels"; "slack"; "reply_mode" ]
    (Runtime_config.to_json cfg)

let check_mode name expected cfg =
  Alcotest.(check (option string))
    name
    (Some (Yojson.Safe.to_string (`String expected)))
    (Option.map Yojson.Safe.to_string (mode cfg))

let test_config () =
  check_mode "absent defaults to mention" "mention" (C.parse_slack []);
  check_mode "all" "all" (C.parse_slack [ ("reply_mode", `String "all") ]);
  check_mode "mention" "mention"
    (C.parse_slack [ ("reply_mode", `String "mention") ])

let test_invalid () =
  Lwt_main.run
    (let invalid = "secret-invalid-reply-mode" in
     let* cfg, logs =
       Test_slack_standing_order_support.with_log_capture (fun () ->
           Lwt.return (C.parse_slack [ ("reply_mode", `String invalid) ]))
     in
     check_mode "invalid defaults to mention" "mention" cfg;
     check_bool "warning names key" true
       (List.exists
          (fun (level, msg) ->
            level = Logs.Warning
            && String_util.contains msg "channels.slack.reply_mode")
          logs);
     check_bool "warning never prints value" false
       (List.exists (fun (_, msg) -> String_util.contains msg invalid) logs);
     let* cfg, _ =
       Test_slack_standing_order_support.with_log_capture (fun () ->
           Lwt.return (C.parse_slack [ ("reply_mode", `Bool true) ]))
     in
     check_mode "non-string defaults to mention" "mention" cfg;
     Lwt.return_unit)

let test_config_set () =
  Test_helpers.with_temp_home (fun _ ->
      let cfg = C.parse_slack [] in
      (match
         Config_set.write_json
           (Config_set.config_path ())
           (Runtime_config.to_json cfg)
       with
      | Ok () -> ()
      | Error e -> Alcotest.fail e);
      Alcotest.(check string)
        "config set succeeds" "Set channels.slack.reply_mode = all"
        (Config_set.set_value "channels.slack.reply_mode" "all");
      let json =
        match Config_set.load_json (Config_set.config_path ()) with
        | Ok json -> json
        | Error e -> Alcotest.fail e
      in
      check_mode "round trip" "all"
        (Config_loader.parse_config ~resolve_secrets:false json))

let test_pure () =
  let silent ?(mode = Slack_config_types.Mention) ?(channel = "C123")
      ?(reply = true) ?(interrupt = false) ?(current = "1100.000001")
      ?(text = "more?") ?(untrusted = fun _ -> false) thread =
    Slack_reply_gate.silent ~reply_mode:mode ~channel ~bot:"UBOT" ~reply
      ~interrupt ~current ~text ~thread ~untrusted
  in
  let check name expected actual = Alcotest.(check bool) name expected actual in
  check "top-level cannot continue" true
    (silent ~reply:false [ own_reply "980.000001" ]);
  check "newest earlier is selected numerically, irrespective of order" false
    (silent [ own_reply "980.000001"; message ~ts:"900.000001" "old human" ]);
  check "equal timestamps do not establish an earlier bot reply" true
    (silent [ own_reply "1100.000001" ]);
  check "future messages do not hide the newest earlier message" false
    (silent
       [ own_reply "980.000001"; message ~ts:"1200.000001" "future human" ]);
  check "malformed timestamps cannot establish continuation" true
    (silent [ own_reply "bad" ]);
  check "missing history stays silent" true (silent []);
  check "exactly fifteen minutes continues" false
    (silent [ own_reply "200.000001" ]);
  check "exactly fifteen minutes at epoch scale continues" false
    (silent ~current:"1793840275.123457" [ own_reply "1793839375.123457" ]);
  check "one microsecond past the cutoff is stale" true
    (silent ~current:"1100.000002" [ own_reply "200.000001" ]);
  check "one microsecond past the cutoff at epoch scale is stale" true
    (silent ~current:"1793840275.123458" [ own_reply "1793839375.123457" ]);
  check "malformed current timestamp stays silent" true
    (silent ~current:"nan" [ own_reply "980.000001" ]);
  check "labeled other user blocks continuation" true
    (silent ~text:"<@W456|person> help" [ own_reply "980.000001" ]);
  check "@here blocks continuation" true
    (silent ~text:"<!here> anyone?" [ own_reply "980.000001" ]);
  check "user group blocks continuation" true
    (silent ~text:"<!subteam^S123|@team> ideas?" [ own_reply "980.000001" ]);
  check "a date token is not a mention" false
    (silent ~text:"by <!date^1700000000^{date}|today>?"
       [ own_reply "980.000001" ]);
  check "clawq's relay post is not a reply to the humans" true
    (silent [ Test_slack_event_fixtures.own_relay ~ts:"980.000001" () ]);
  check "a recorded relay post is not a reply to the humans" true
    (silent ~untrusted:(fun _ -> true) [ own_reply "980.000001" ]);
  check "owner's interrupt is addressed to clawq" false
    (silent ~interrupt:true []);
  check "legacy all bypasses mention checks" false
    (silent ~mode:Slack_config_types.All []);
  check "DM bypasses mention checks" false (silent ~channel:"D123" [])

let case name f = (name, `Quick, f)

let suite =
  [
    case "top-level channel message without a mention starts no turn"
      test_top_silent;
    case "top-level channel message mentioning clawq starts a turn"
      test_top_mention;
    case "huddle thread reply without a mention starts no turn"
      test_huddle_silent;
    case "thread reply right after clawq's reply continues without a mention"
      (after_own "980.000001" ~turn:true);
    case "thread reply after a stale clawq reply starts no turn"
      (after_own "140.000001" ~turn:false);
    case
      "thread reply that mentions someone else starts no turn even after \
       clawq's reply"
      (after_own ~text:"<@U456> your thoughts?" "980.000001" ~turn:false);
    case "DM without a mention starts a turn" test_dm;
    case "reply_mode all keeps the old behaviour" test_all;
    case "pause SO-n works without a mention in mention mode" test_pause;
    case "reply_mode config defaults and all" test_config;
    case "invalid reply_mode warns without its value" test_invalid;
    case "config set channels.slack.reply_mode all round-trips" test_config_set;
    case "reply_mode change applies on reload without restart" test_reload;
    case "top-level slash command requires a mention" test_command;
    case "newest human reply ends implicit continuation"
      (test_followup ~text:"followup"
         ~posts:
           [
             huddle_root;
             own_reply "980.000001";
             message ~ts:"1000.000001" "human";
           ]
         ~turn:false);
    case "future bot reply cannot authorize an earlier message"
      (test_followup ~text:"followup"
         ~posts:[ huddle_root; own_reply "1200.000001" ]
         ~turn:false);
    case "continuation includes exactly fifteen minutes"
      (after_own ~text:"followup" "200.000001" ~turn:true);
    case "clawq mention overrides other mentions"
      (after_own ~text:"<@U456> <@UBOT|clawq> help" "140.000001" ~turn:true);
    case "relay post by clawq does not open a continuation"
      (test_followup ~text:"followup"
         ~posts:
           [
             huddle_root;
             Test_slack_event_fixtures.own_relay ~channel:"C123"
               ~ts:"980.000001" ();
           ]
         ~turn:false);
    case "run owner's bang interrupt keeps its instruction in mention mode"
      test_bang;
    case "pure reply decision and chronological boundaries" test_pure;
  ]
