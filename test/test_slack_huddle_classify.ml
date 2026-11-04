open Test_slack_huddle_fixtures
module F = Test_slack_event_fixtures
module C = Slack_huddle_classify

let ctx =
  { C.empty_context with self_user_id = Some "UBOT"; targets = [ F.target ] }

let classify ?(ctx = ctx) e = C.classify ctx (F.envelope e)
let irrelevant ?ctx e = classify ?ctx e = C.Irrelevant
let check_str = Alcotest.(check string)
let check_ids = Alcotest.(check (list string))
let check_str_opt = Alcotest.(check (option string))
let check_int_opt = Alcotest.(check (option int))

let check_irrelevant label cases =
  List.iter (fun (case, e) -> check_bool (label ^ ": " ^ case) true e) cases

let room_of label e =
  match classify e with
  | C.Huddle_seen { room; _ } -> room
  | _ -> Alcotest.fail (label ^ ": expected Huddle_seen")

let check_room label (e : C.room) (a : C.room) =
  let l field = label ^ ": " ^ field in
  check_str (l "room_id") e.C.room_id a.C.room_id;
  check_bool (l "has_ended") e.C.has_ended a.C.has_ended;
  check_int_opt (l "date_start") e.C.date_start a.C.date_start;
  check_int_opt (l "date_end") e.C.date_end a.C.date_end;
  check_str_opt (l "thread_root_ts") e.C.thread_root_ts a.C.thread_root_ts;
  check_str_opt (l "canvas_thread_ts") e.C.canvas_thread_ts a.C.canvas_thread_ts;
  check_ids (l "attached_file_ids") e.C.attached_file_ids a.C.attached_file_ids;
  check_str_opt (l "transcript_file_id") e.C.transcript_file_id
    a.C.transcript_file_id

(* The P0 root shape: R0ROOM1, open, started 1700000000, a transcript hint. *)
let root_room =
  {
    C.room_id = "R0ROOM1";
    has_ended = false;
    date_start = Some 1700000000;
    date_end = None;
    thread_root_ts = Some "1700000000.000100";
    canvas_thread_ts = Some "1700000000.000100";
    attached_file_ids = [];
    transcript_file_id = Some "F0TRANSCRIPT";
  }

let test_huddle_roots () =
  (match classify (F.huddle_root ()) with
  | C.Huddle_seen { channel; root_ts; file_ids; room } ->
      check_str "root channel" "C0TEST" channel;
      check_str "root ts" "1700000000.000100" root_ts;
      check_ids "root files carry the canvas" [ "F0CANVAS" ] file_ids;
      check_room "P0 root" root_room room
  | _ -> Alcotest.fail "P0 root: expected Huddle_seen");
  check_bool "subtype alone identifies a huddle" false
    (irrelevant (F.huddle_root ~room:(F.room ~call_family:None ()) ()));
  check_bool "call_family alone identifies a huddle" false
    (irrelevant (F.huddle_root ~subtype:None ()));
  (match classify (F.thread_started_huddle ()) with
  | C.Huddle_seen { root_ts; room; _ } ->
      check_str "thread-started root ts" "1700000500.000300" root_ts;
      check_str_opt "thread-started thread root" (Some "1699990000.000100")
        room.C.thread_root_ts
  | _ -> Alcotest.fail "thread-started: expected Huddle_seen");
  check_room "ended edit"
    { root_room with C.has_ended = true; date_end = Some 1700003600 }
    (room_of "ended edit"
       (F.root_changed
          (F.huddle_root
             ~room:(F.room ~has_ended:true ~date_end:1700003600 ())
             ())));
  check_bool "date_end alone means ended" true
    (room_of "date_end alone"
       (F.huddle_root ~room:(F.room ~date_end:1700003600 ()) ()))
      .C.has_ended;
  List.iter
    (fun (label, transcript) ->
      check_str_opt label None
        (room_of label (F.huddle_root ~room:(F.room ~transcript ()) ()))
          .C.transcript_file_id)
    [ ("no transcript id", None); ("lowercase transcript id", Some "f0bad") ];
  check_ids "attached ids: valid, deduplicated" [ "F0SHARED" ]
    (room_of "attached ids"
       (F.huddle_root
          ~room:(F.room ~attached:[ "F0SHARED"; "f0bad"; "F0SHARED" ] ())
          ()))
      .C.attached_file_ids;
  check_irrelevant "not a huddle"
    [
      ( "lowercase room id",
        irrelevant (F.huddle_root ~room:(F.room ~id:"r0room1" ()) ()) );
      ( "deleted edit",
        irrelevant
          (update
             (F.root_changed (F.huddle_root ()))
             [ ("subtype", str "message_deleted") ]) );
    ]

let test_over_long () =
  check_irrelevant "over-long id"
    [
      ( "65-char room id",
        irrelevant
          (F.huddle_root ~room:(F.room ~id:("R" ^ String.make 64 'A') ()) ()) );
      ( "65-char channel",
        irrelevant (F.huddle_root ~channel:("C" ^ String.make 64 'A') ()) );
    ];
  List.iter
    (fun (label, v, expected) ->
      check_int_opt label expected
        (room_of label
           (F.huddle_root ~room:(update (F.room ()) [ ("date_start", v) ]) ()))
          .C.date_start)
    [
      ("digit string", str "1700000000", Some 1700000000);
      ("intlit", `Intlit "1700000000", Some 1700000000);
      ("integral float", `Float 1700000000., Some 1700000000);
      ("exponent string", str "17e8", None);
      ("signed string", str "-5", None);
      ("empty string", str "", None);
      ("fractional float", `Float 1.5, None);
      ("object", obj [], None);
    ]

let test_notes_ready () =
  (match classify (F.notes_ready ()) with
  | C.Notes_ready { channel; thread_ts; ts; file_ids; canvas_file_id; variant }
    ->
      check_str "notes channel" "C0TEST" channel;
      check_str "notes thread" "1700000000.000100" thread_ts;
      check_str "notes ts" "1700003900.000400" ts;
      check_ids "notes files" [ "F0CANVAS" ] file_ids;
      (* The canvas belongs to U0STARTER, not Slackbot: ownership is unread. *)
      check_str "canvas id" "F0CANVAS" canvas_file_id;
      check_str "variant" "notes_and_transcript"
        (C.notes_variant_to_string variant)
  | _ -> Alcotest.fail "notes ready: expected Notes_ready");
  check_bool "explicit null bot_id and subtype" true
    (match
       classify
         (update (F.notes_ready ()) [ ("bot_id", `Null); ("subtype", `Null) ])
     with
    | C.Notes_ready _ -> true
    | _ -> false);
  (match
     classify
       (F.notes_ready
          ~files:[ F.md_file (); F.canvas_file () ]
          ~text:"AI huddle notes are ready." ())
   with
  | C.Notes_ready { canvas_file_id; variant; _ } ->
      check_str "canvas chosen by mimetype" "F0CANVAS" canvas_file_id;
      check_str "notes-only variant" "notes_only"
        (C.notes_variant_to_string variant)
  | _ -> Alcotest.fail "notes only: expected Notes_ready");
  check_irrelevant "spoofed notes ready"
    [
      ("human imitation", irrelevant (F.notes_ready ~user:"U123" ()));
      ("bot_id present", irrelevant (F.notes_ready ~bot_id:"B1" ()));
      ("no thread_ts", irrelevant (F.notes_ready ~thread_ts:None ()));
      ("no files", irrelevant (F.notes_ready ~files:[] ()));
      ( "no valid file id",
        irrelevant
          (F.notes_ready
             ~files:[ obj [ ("id", str "f1") ]; obj [ ("id", str "X1") ] ]
             ()) );
      ( "room and Slackbot only in blocks and metadata",
        irrelevant
          (update (F.human "x")
             [
               ("blocks", arr [ obj [ ("room", obj [ ("id", str "R1") ]) ] ]);
               ("metadata", obj [ ("user", str "USLACKBOT") ]);
             ]) );
    ]

let is_own_relay ?ctx e =
  match classify ?ctx e with C.Own_relay _ -> true | _ -> false

let test_own_relay () =
  (match classify (F.own_relay ()) with
  | C.Own_relay { channel; ts; file_ids; nonce } ->
      check_str "relay channel" "C0TEST" channel;
      check_str "relay ts" "1700004000.000500" ts;
      check_ids "relay files" [ "F0MD" ] file_ids;
      check_str "relay nonce" F.relay_nonce nonce
  | _ -> Alcotest.fail "own relay: expected Own_relay");
  check_bool "file_share subtype" true
    (is_own_relay (F.own_relay ~subtype:"file_share" ()));
  check_bool "no bot_id" true (is_own_relay (F.own_relay ~bot_id:None ()));
  check_bool "thread_ts equal to ts" true
    (is_own_relay (F.own_relay ~thread_ts:"1700004000.000500" ()));
  check_irrelevant "not an own relay"
    [
      ("no self user id", irrelevant ~ctx:C.empty_context (F.own_relay ()));
      ("no footer", irrelevant (F.own_relay ~footer:None ()));
      ("other user", irrelevant (F.own_relay ~user:"U123" ()));
      ( "thread reply",
        irrelevant (F.own_relay ~thread_ts:"1700000000.000100" ()) );
    ]

let is_target_reply ?ctx e =
  match classify ?ctx e with C.Target_reply _ -> true | _ -> false

let test_target () =
  (match classify (F.claude_reply ()) with
  | C.Target_reply
      {
        channel;
        thread_ts;
        ts;
        bot_id;
        app_id;
        username;
        kind_if_first;
        kind_if_later;
      } ->
      check_str "reply channel" "C0TEST" channel;
      check_str "reply thread" "1700004000.000500" thread_ts;
      check_str "reply ts" "1700004010.000600" ts;
      check_str "reply bot" "B0CLAUDE" bot_id;
      check_str_opt "reply app" (Some "A0CLAUDE") app_id;
      check_str_opt "reply username" None username;
      check_bool "first reply kind" true (kind_if_first = C.Reply);
      check_bool "later reply kind" true (kind_if_later = C.Reply)
  | _ -> Alcotest.fail "claude reply: expected Target_reply");
  check_bool "top-level app id agrees" true
    (is_target_reply (F.claude_reply ~top_app_id:"A0CLAUDE" ()));
  List.iter
    (fun (label, e, expected) ->
      match classify e with
      | C.Target_reply { username; _ } -> check_str_opt label expected username
      | _ -> Alcotest.fail (label ^ ": expected Target_reply"))
    [
      ("string username", F.claude_reply ~username:"Claude" (), Some "Claude");
      ("null username", update (F.claude_reply ()) [ ("username", `Null) ], None);
    ];
  check_irrelevant "not a target reply"
    [
      ( "bot_profile app mismatch",
        irrelevant (F.claude_reply ~app_id:"A0EVIL" ()) );
      ( "top-level app mismatch",
        irrelevant (F.claude_reply ~top_app_id:"A0EVIL" ()) );
      ("top-level message", irrelevant (F.claude_reply ~thread_ts:None ()));
      ("other bot", irrelevant (F.claude_reply ~bot_id:"B0OTHER" ()));
      ("no targets", irrelevant ~ctx:C.empty_context (F.claude_reply ()));
    ];
  check_bool "message reaction" true
    (classify (F.reaction ())
    = C.Target_reaction
        {
          channel = Some "C0TEST";
          user = "U0CLAUDE";
          item_ts = Some "1700004000.000500";
          item_file = None;
        });
  let fr =
    F.reaction ~item:(obj [ ("type", str "file"); ("file", str "F0MD") ]) ()
  in
  check_bool "file reaction without channel" true
    (classify fr
    = C.Target_reaction
        {
          channel = None;
          user = "U0CLAUDE";
          item_ts = None;
          item_file = Some "F0MD";
        });
  check_str_opt "file reaction event channel" None
    (C.event_channel (F.envelope fr));
  check_irrelevant "not a target reaction"
    [
      ( "message reaction without channel",
        irrelevant
          (F.reaction
             ~item:
               (obj
                  [ ("type", str "message"); ("ts", str "1700004000.000500") ])
             ()) );
      ("other user", irrelevant (F.reaction ~user:"U123" ()));
    ]

let test_text_kinds () =
  let m =
    {
      C.first_reply = [ ("Rate limited.", C.Rate_limited) ];
      any_reply = [ ("Spend limit reached", C.Spend_limit) ];
    }
  in
  let k ?(first = true) t = C.text_kind m ~first t in
  check_bool "no matchers: reply" true
    (C.text_kind C.no_matchers ~first:true "ok" = C.Reply);
  check_bool "first-reply notice, trimmed" true
    (k " Rate limited. " = C.Notice C.Rate_limited);
  check_bool "first-reply notice ignored later" true
    (k ~first:false "Rate limited." = C.Reply);
  check_bool "any-reply notice first" true
    (k "Spend limit reached" = C.Notice C.Spend_limit);
  check_bool "any-reply notice later" true
    (k ~first:false "Spend limit reached" = C.Notice C.Spend_limit);
  check_bool "exact match only" true (k "Spend limit reached!" = C.Reply)

let test_posture_malformed () =
  check_ids "posture event types"
    [
      "member_joined_channel";
      "member_left_channel";
      "channel_shared";
      "channel_unshared";
    ]
    C.posture_event_types;
  List.iter
    (fun kind ->
      check_bool kind true
        (classify (F.posture kind) = C.Posture_change { channel = "C0TEST" }))
    C.posture_event_types;
  check_irrelevant "irrelevant event"
    [
      ("channel_rename", irrelevant (F.posture "channel_rename"));
      ( "DM posture",
        irrelevant (F.posture ~channel:"D0DM" "member_joined_channel") );
      ("DM huddle", irrelevant (F.huddle_root ~channel:"D0DM" ()));
      ( "edit without a message object",
        irrelevant
          (update (F.root_changed (F.huddle_root ())) [ ("message", str "x") ])
      );
      ("null envelope", C.classify ctx `Null = C.Irrelevant);
      ("block_actions", C.classify ctx F.block_actions = C.Irrelevant);
      ( "string event",
        C.classify ctx
          (obj [ ("type", str "event_callback"); ("event", str "x") ])
        = C.Irrelevant );
      ( "wrong-typed fields",
        C.classify ctx
          (F.envelope
             (obj
                [
                  ("type", str "message");
                  ("channel", str "C0TEST");
                  ("ts", `Int 5);
                  ("room", arr []);
                  ("files", str "F1");
                  ("user", obj []);
                ]))
        = C.Irrelevant );
    ];
  check_str_opt "reaction event channel" (Some "C0TEST")
    (C.event_channel (F.envelope (F.reaction ())));
  check_str_opt "huddle channel" (Some "C0TEST")
    (C.channel_of (classify (F.huddle_root ())))

let suite =
  [
    ("huddle roots and edits", `Quick, test_huddle_roots);
    ("over-long ids and numeric strings", `Quick, test_over_long);
    ("notes ready and spoofs", `Quick, test_notes_ready);
    ("own relay structure", `Quick, test_own_relay);
    ("target replies and reactions", `Quick, test_target);
    ("notice text kinds", `Quick, test_text_kinds);
    ( "posture events and malformed shapes are irrelevant",
      `Quick,
      test_posture_malformed );
  ]
