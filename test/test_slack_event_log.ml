open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads
module F = Test_slack_event_fixtures

let rule =
  { Slack_config_types.text_channel_id = "C0TEST"; text_bot_id = "B0CLAUDE" }

let record ?(rules = []) e =
  Slack_event_log.record ~now:1.0 ~rules (F.envelope e)

let ev r = get "event" r
let has r s = contains (Yojson.Safe.to_string r) s
let keys json = List.map fst (Yojson.Safe.Util.to_assoc json)
let lacks json key = not (List.mem key (keys json))

let test_human_text () =
  let r = record ~rules:[ rule ] (F.human "the secret plan") in
  check_bool "no human text" false (has r "secret plan");
  check_bool "user kept" true (get "user" (ev r) = str "U123");
  check_bool "channel kept" true (get "channel" (ev r) = str "C0TEST")

let test_room () =
  let r = record (F.huddle_root ()) in
  let room = get "room" (ev r) in
  check_bool "id" true (get "id" room = str "R0ROOM1");
  check_bool "call_family" true (get "call_family" room = str "huddle");
  check_bool "has_ended" true (get "has_ended" room = `Bool false);
  check_bool "date_start" true (get "date_start" room = `Int 1700000000);
  check_bool "attached_file_ids" true (get "attached_file_ids" room = arr []);
  check_bool "transcript_file_id" true
    (get "transcript_file_id" room = str "F0TRANSCRIPT");
  check_bool "key names" true
    (List.mem
       (str "participant_history")
       (Yojson.Safe.Util.to_list (get "keys" room)));
  check_bool "no room name" false (has r "Secret planning");
  check_bool "no block text" false (has r "A huddle started")

let test_message_changed () =
  let r =
    record
      (F.root_changed
         (F.huddle_root
            ~room:(F.room ~has_ended:true ~date_end:1700003600 ())
            ()))
  in
  check_bool "nested room ended" true
    (get "has_ended" (get "room" (get "message" (ev r))) = `Bool true);
  check_bool "previous_message kept" true
    (get "previous_message" (ev r) <> `Null)

let test_files_and_bots () =
  let r = record (F.notes_ready ()) in
  check_bool "canvas file metadata" true
    (get "files" (ev r)
    = arr
        [
          obj
            [
              ("id", str "F0CANVAS");
              ("mimetype", str "application/vnd.slack-docs");
              ("filetype", str "quip");
              ("user", str "U0STARTER");
            ];
        ]);
  List.iter
    (fun s -> check_bool ("no " ^ s) false (has r s))
    [ "SECRET"; "Huddle AI notes"; "are ready" ];
  let r =
    record (update (F.human "x") [ ("files", arr [ F.transcript_file () ]) ])
  in
  check_bool "transcript file metadata" true
    (get "files" (ev r)
    = arr
        [
          obj
            [
              ("id", str "F0TRANSCRIPT");
              ("mimetype", str "application/vnd.slack-huddle-transcript");
              ("filetype", str "huddle_transcript");
              ("user", str "U0STARTER");
            ];
        ]);
  check_bool "no spoken line" false (has r "SECRET SPOKEN");
  check_bool "no transcription key" false (has r "huddle_transcription");
  let e =
    ev
      (record
         (F.claude_reply ~top_app_id:"A0CLAUDE" ~username:"Claude [Ingest]" ()))
  in
  check_bool "bot_id" true (get "bot_id" e = str "B0CLAUDE");
  check_bool "bot_profile_app_id" true
    (get "bot_profile_app_id" e = str "A0CLAUDE");
  check_bool "app_id" true (get "app_id" e = str "A0CLAUDE");
  check_bool "username" true (get "username" e = str "Claude [Ingest]");
  check_bool "no username key (P0-3)" true
    (lacks (ev (record (F.claude_reply ()))) "username");
  check_bool "null username omitted" true
    (lacks
       (ev (record (update (F.claude_reply ()) [ ("username", `Null) ])))
       "username");
  let e = ev (record (F.human "x")) in
  List.iter
    (fun k -> check_bool ("absent " ^ k) true (lacks e k))
    [ "item"; "bot_profile_app_id"; "files"; "room"; "bot_id" ]

let test_reaction () =
  let e = ev (record (F.reaction ())) in
  check_bool "reaction" true (get "reaction" e = str "eyes");
  check_bool "user" true (get "user" e = str "U0CLAUDE");
  check_bool "item_user" true (get "item_user" e = str "UBOT");
  check_bool "item" true
    (get "item" e
    = obj
        [
          ("type", str "message");
          ("ts", str "1700004000.000500");
          ("channel", str "C0TEST");
        ])

let test_file_events () =
  let r =
    record
      (F.file_event ~channel_id:"C0TEST" ~user_id:"U0STARTER" "file_shared")
  in
  let e = ev r in
  check_bool "type" true (get "type" e = str "file_shared");
  check_bool "file_id" true (get "file_id" e = str "F0CANVAS");
  check_bool "file_obj_id" true (get "file_obj_id" e = str "F0CANVAS");
  check_bool "channel_id" true (get "channel_id" e = str "C0TEST");
  check_bool "user_id" true (get "user_id" e = str "U0STARTER");
  List.iter
    (fun s -> check_bool ("no " ^ s) false (has r s))
    [ "Huddle AI notes"; "SECRET"; "preview" ];
  let e = ev (record (F.file_event ~with_file:false "file_deleted")) in
  check_bool "deleted file_id" true (get "file_id" e = str "F0CANVAS");
  check_bool "deleted event_ts" true (get "event_ts" e <> `Null);
  check_bool "no file_obj_id" true (lacks e "file_obj_id")

let test_text_exception () =
  let t ?(rules = [ rule ]) ?(channel = "C0TEST") ?(bot_id = "B0CLAUDE") () =
    has
      (record ~rules
         (F.claude_reply ~channel ~bot_id ~text:"Spend limit reached" ()))
      "Spend limit reached"
  in
  check_bool "exact channel and bot" true (t ());
  check_bool "other channel" false (t ~channel:"C0OTHER" ());
  check_bool "other bot" false (t ~bot_id:"B0OTHER" ());
  check_bool "no rules" false (t ~rules:[] ());
  check_bool "edited reply in the channel" true
    (has
       (record ~rules:[ rule ]
          (F.root_changed (F.claude_reply ~text:"Spend limit reached" ())))
       "Spend limit reached")

let test_secrets () =
  let r =
    Slack_event_log.record ~now:1.0 ~rules:[]
      (update
         (F.envelope (F.human "x"))
         [
           ("token", str "VERIFICATION");
           ("authorizations", arr [ obj [ ("user_id", str "UBOT") ] ]);
         ])
  in
  check_bool "no token" false (has r "VERIFICATION");
  check_bool "no authorizations" false (has r "authorizations");
  let r = Slack_event_log.record ~now:1.0 ~rules:[] F.block_actions in
  check_bool "interactive type" true
    (get "type" (get "envelope" r) = str "block_actions");
  check_bool "interactive team" true
    (get "team_id" (get "envelope" r) = str "T123");
  check_bool "no action value" false (has r "opaque-secret");
  check_bool "no interactive token" false (has r "VERIFICATION")

let test_malformed () =
  let r = Slack_event_log.record ~now:1.0 ~rules:[ rule ] `Null in
  check_bool "null payload has no event" true (lacks r "event");
  let r =
    Slack_event_log.record ~now:1.0 ~rules:[ rule ]
      (obj [ ("type", str "event_callback"); ("event", str "x") ])
  in
  check_bool "non-object event omitted" true (lacks r "event");
  let r =
    record ~rules:[ rule ]
      (obj
         [
           ("type", str "message");
           ("room", arr []);
           ("files", str "F1");
           ("user", obj [ ("real_name", str "Leaky") ]);
           ("item", `Int 3);
         ])
  in
  check_bool "no nested value" false (has r "Leaky");
  check_bool "only well-shaped keys" true (keys (ev r) = [ "type" ])

let test_append () =
  Test_helpers.with_temp_home (fun dir ->
      let path = Filename.concat dir "events.jsonl" in
      Slack_event_log.append ~path (obj [ ("n", `Int 1) ]);
      Slack_event_log.append ~path (obj [ ("n", `Int 2) ]);
      Alcotest.(check int) "two lines" 2 (List.length (F.read_lines path));
      Alcotest.(check int)
        "owner-only file" 0o600
        ((Unix.stat path).st_perm land 0o777);
      Slack_event_log.append ~max_bytes:1 ~path (obj [ ("n", `Int 3) ]);
      Alcotest.(check int)
        "rotated lines" 2
        (List.length (F.read_lines (path ^ ".1")));
      Alcotest.(check int) "fresh file" 1 (List.length (F.read_lines path)))

let test_config () =
  let base =
    [
      ("bot_token", str "xoxb-x");
      ("app_token", str "xapp-x");
      ("socket_mode", `Bool true);
    ]
  in
  let parse fields =
    Option.get
      (Config_loader.parse_config ~resolve_secrets:false
         (obj [ ("channels", obj [ ("slack", obj fields) ]) ]))
        .channels
        .slack
  in
  let r c b = obj [ ("channel", str c); ("bot_id", str b) ] in
  let sc =
    parse
      (base
      @ [
          ( "event_log",
            obj
              [
                ("enabled", `Bool true);
                ( "capture_text_from",
                  arr
                    [
                      r "C0TEST" "B0CLAUDE";
                      r "*" "B0CLAUDE";
                      r "D0DM" "B0CLAUDE";
                      r "C0TEST" "U0CLAUDE";
                    ] );
              ] );
        ])
  in
  check_bool "enabled; only exact C/G + B rules" true
    (sc.event_log.enabled && sc.event_log.capture_text_from = [ rule ]);
  check_bool "default off" true
    ((parse base).event_log = Slack_config_types.default_event_log);
  check_bool "non-object is default" true
    ((parse (base @ [ ("event_log", str "on") ])).event_log
   = Slack_config_types.default_event_log);
  let cfg =
    {
      Runtime_config.default with
      channels = { Runtime_config.default.channels with slack = Some sc };
    }
  in
  let round =
    Config_loader.parse_config ~resolve_secrets:false
      (Runtime_config.to_json cfg)
  in
  check_bool "json roundtrip" true
    ((Option.get round.channels.slack).event_log = sc.event_log);
  check_bool "settable" true
    (Config_set.validate_set_path
       [ "channels"; "slack"; "event_log"; "enabled" ]
       Config_set.config_schema)

let set_log (base : fixture) enabled =
  let cfg = base.manager.config in
  let sc = Option.get cfg.channels.slack in
  base.manager.config <-
    {
      cfg with
      channels =
        {
          cfg.channels with
          slack =
            Some
              {
                sc with
                event_log =
                  { Slack_config_types.enabled; capture_text_from = [] };
              };
        };
    }

let log () = F.read_lines (Slack_event_log.default_path ())

let test_tap () =
  with_ingress (fun base calls _ seen _ run ->
      let* _ = run (F.huddle_root ()) in
      check_bool "off by default" true (log () = []);
      set_log base true;
      let* _ = run ~id:"Ev2" (F.huddle_root ~ts:"1700000001.000100" ()) in
      check_bool "huddle root makes no Slack call" true (!calls = []);
      let* _ = run ~id:"Ev3" (message "please help") in
      check_bool "human turn still runs" true (!seen <> []);
      Alcotest.(check int) "both enabled events logged" 2 (List.length (log ()));
      check_bool "turn text not logged" false
        (List.exists (fun l -> contains l "please help") (log ()));
      set_log base false;
      let* _ = run ~id:"Ev4" (F.huddle_root ~ts:"1700000002.000100" ()) in
      Alcotest.(check int)
        "runtime toggle takes effect" 2
        (List.length (log ()));
      Lwt.return_unit)

let suite =
  [
    ("human text never logged", `Quick, test_human_text);
    ("room metadata and key names only", `Quick, test_room);
    ("message_changed nested metadata", `Quick, test_message_changed);
    ("file and bot metadata only", `Quick, test_files_and_bots);
    ("reaction metadata", `Quick, test_reaction);
    ("file events keep ids only", `Quick, test_file_events);
    ("text exception exact channel and bot", `Quick, test_text_exception);
    ("secrets and action values never logged", `Quick, test_secrets);
    ("malformed payloads never raise", `Quick, test_malformed);
    ("append rotation and permissions", `Quick, test_append);
    ("config load, roundtrip and set path", `Quick, test_config);
    ( "tap off by default, logs, keeps routing, runtime toggle takes effect",
      `Quick,
      test_tap );
  ]
