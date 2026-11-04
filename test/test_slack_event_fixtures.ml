(* Slack event payloads shared by the huddle tests. Shapes follow the P0 live
   captures; values containing "SECRET", "Secret" or "A huddle started" are
   planted only to prove the event log and classifier drop them. An optional
   argument left out, or set to [None], omits its key. *)
open Test_slack_huddle_fixtures

let opt key = function None -> [] | Some v -> [ (key, str v) ]

let envelope ?(team = "T123") ?(app = "A123") ?(id = "Ev1") event =
  obj
    [
      ("type", str "event_callback");
      ("team_id", str team);
      ("api_app_id", str app);
      ("event_id", str id);
      ("event_time", `Int 1700000000);
      ("event", event);
    ]

let room ?(id = "R0ROOM1") ?(call_family = Some "huddle") ?(has_ended = false)
    ?(date_start = 1700000000) ?(date_end = 0)
    ?(thread_root_ts = "1700000000.000100") ?(attached = [])
    ?(transcript = Some "F0TRANSCRIPT") () =
  obj
    ([
       ("id", str id);
       ("name", str "Secret planning");
       ("created_by", str "U123");
       ("date_start", `Int date_start);
       ("date_end", `Int date_end);
       ("participant_history", arr [ str "U123" ]);
       ("thread_root_ts", str thread_root_ts);
       ("canvas_thread_ts", str thread_root_ts);
       ("channels", arr [ str "C0TEST" ]);
       ("has_ended", `Bool has_ended);
       ("attached_file_ids", arr (List.map str attached));
     ]
    @ opt "call_family" call_family
    @ opt "transcript_file_id" transcript)

let canvas_file ?(id = "F0CANVAS") ?(user = "U0STARTER") () =
  obj
    [
      ("id", str id);
      ("name", str "Huddle AI notes");
      ("title", str "Huddle AI notes & transcript");
      ("mimetype", str "application/vnd.slack-docs");
      ("filetype", str "quip");
      ("user", str user);
      ("preview", str "SECRET NOTES PREVIEW");
    ]

let huddle_root ?(channel = "C0TEST") ?(ts = "1700000000.000100")
    ?(subtype = Some "huddle_thread") ?(room = room ())
    ?(files = [ canvas_file () ]) () =
  obj
    ([
       ("type", str "message");
       ("user", str "USLACKBOT");
       ("text", str "");
       ("ts", str ts);
       ("channel", str channel);
       ("room", room);
       ("files", arr files);
       ( "blocks",
         arr
           [
             obj [ ("type", str "rich_text"); ("text", str "A huddle started") ];
           ] );
     ]
    @ opt "subtype" subtype)

let root_changed ?(channel = "C0TEST") ?(ts = "1700003600.000200") message =
  obj
    [
      ("type", str "message");
      ("subtype", str "message_changed");
      ("hidden", `Bool true);
      ("channel", str channel);
      ("ts", str ts);
      ("event_ts", str ts);
      ("message", message);
      ("previous_message", message);
    ]

(* Third-party shape for a huddle started inside a thread (P0-5 open). *)
let thread_started_huddle () =
  update
    (huddle_root ~subtype:(Some "thread_broadcast") ~ts:"1700000500.000300"
       ~room:(room ~id:"R0ROOM2" ~thread_root_ts:"1699990000.000100" ())
       ())
    [ ("thread_ts", str "1699990000.000100") ]

let transcript_file ?(id = "F0TRANSCRIPT") () =
  obj
    [
      ("id", str id);
      ("mimetype", str "application/vnd.slack-huddle-transcript");
      ("filetype", str "huddle_transcript");
      ("user", str "U0STARTER");
      ( "huddle_transcription",
        obj
          [
            ("channel_id", str "C0TEST");
            ( "lines",
              arr
                [
                  obj
                    [
                      ("contents", str "SECRET SPOKEN LINE");
                      ("start_time_ms", `Int 0);
                      ("line_id", str "L1");
                      ("user_id", str "U123");
                    ];
                ] );
          ] );
    ]

let md_file ?(id = "F0MD") () =
  obj
    [
      ("id", str id);
      ("name", str "notes.md");
      ("title", str "notes");
      ("mimetype", str "text/markdown");
      ("filetype", str "markdown");
      ("user", str "UBOT");
      ("size", `Int 10);
      ("created", `Int 1700000000);
      ("preview", str "SECRET MD PREVIEW");
      ("preview_plain_text", str "SECRET PLAIN");
      ("lines", `Int 3);
      ("title_blocks", arr [ obj [ ("text", str "SECRET TITLE BLOCK") ] ]);
      ("url_private", str "https://files.slack.com/files-pri/T123-F0MD/notes.md");
    ]

let notes_ready ?(channel = "C0TEST") ?(user = "USLACKBOT") ?bot_id
    ?(thread_ts = Some "1700000000.000100") ?(ts = "1700003900.000400")
    ?(files = [ canvas_file () ])
    ?(text =
      "AI huddle notes and transcript are ready. \
       <https://x.slack.com/docs/T123/F0CANVAS|View AI Notes & Transcript>") ()
    =
  obj
    ([
       ("type", str "message");
       ("user", str user);
       ("channel", str channel);
       ("ts", str ts);
       ("text", str text);
       ("files", arr files);
     ]
    @ opt "thread_ts" thread_ts @ opt "bot_id" bot_id)

let claude_reply ?(channel = "C0TEST") ?(bot_id = "B0CLAUDE")
    ?(app_id = "A0CLAUDE") ?top_app_id ?username
    ?(thread_ts = Some "1700004000.000500") ?(ts = "1700004010.000600")
    ?(text = "ok") () =
  obj
    ([
       ("type", str "message");
       ("user", str "U0CLAUDE");
       ("bot_id", str bot_id);
       ("bot_profile", obj [ ("app_id", str app_id); ("name", str "Claude") ]);
       ("channel", str channel);
       ("ts", str ts);
       ("text", str text);
     ]
    @ opt "app_id" top_app_id @ opt "username" username
    @ opt "thread_ts" thread_ts)

let reaction ?(user = "U0CLAUDE")
    ?(item =
      obj
        [
          ("type", str "message");
          ("channel", str "C0TEST");
          ("ts", str "1700004000.000500");
        ]) () =
  obj
    [
      ("type", str "reaction_added");
      ("user", str user);
      ("reaction", str "eyes");
      ("item_user", str "UBOT");
      ("item", item);
      ("event_ts", str "1700004005.000700");
    ]

let posture ?(channel = "C0TEST") kind =
  obj
    [
      ("type", str kind);
      ("channel", str channel);
      ("user", str "U999");
      ("event_ts", str "1700005000.000800");
    ]

(* file_shared / file_deleted / file_change (P0-5 deletion check). *)
let file_event ?(id = "F0CANVAS") ?(with_file = true) ?channel_id ?user_id kind
    =
  obj
    ([
       ("type", str kind);
       ("file_id", str id);
       ("event_ts", str "1700006000.001000");
     ]
    @ (if with_file then [ ("file", canvas_file ~id ()) ] else [])
    @ opt "channel_id" channel_id
    @ opt "user_id" user_id)

let human ?(channel = "C0TEST") ?thread_ts ?(ts = "1700002000.000900")
    ?(user = "U123") text =
  obj
    ([
       ("type", str "message");
       ("channel", str channel);
       ("user", str user);
       ("ts", str ts);
       ("text", str text);
     ]
    @ opt "thread_ts" thread_ts)

let block_actions =
  obj
    [
      ("type", str "block_actions");
      ("team", obj [ ("id", str "T123") ]);
      ("api_app_id", str "A123");
      ("user", obj [ ("id", str "U123") ]);
      ("token", str "VERIFICATION");
      ( "actions",
        arr
          [
            obj
              [
                ("action_id", str "clawq_stop_run");
                ("value", str "opaque-secret");
              ];
          ] );
    ]

let read_lines path =
  if not (Sys.file_exists path) then []
  else
    In_channel.with_open_text path In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")

let relay_nonce = "0123456789abcdef0123456789abcdef"

(* Clawq's own standing-order relay post (P0-4: no subtype). *)
let own_relay ?(channel = "C0TEST") ?(user = "UBOT") ?(bot_id = Some "BOWN")
    ?subtype ?(ts = "1700004000.000500") ?thread_ts ?(files = [ md_file () ])
    ?(footer = Some (Slack_relay_guard.footer ~nonce:relay_nonce)) () =
  let footer = match footer with None -> "" | Some f -> "\n" ^ f in
  obj
    ([
       ("type", str "message");
       ("user", str user);
       ("channel", str channel);
       ("ts", str ts);
       ("files", arr files);
       ("text", str ("<@U0CLAUDE> Huddle notes from this channel." ^ footer));
     ]
    @ opt "bot_id" bot_id @ opt "subtype" subtype @ opt "thread_ts" thread_ts)
