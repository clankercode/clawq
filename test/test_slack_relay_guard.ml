open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads
module F = Test_slack_event_fixtures
module G = Slack_relay_guard

let reg ?(recorded = false) ?(in_flight = []) ?since () =
  {
    G.recorded_relay = (fun ~channel:_ ~ts:_ -> recorded);
    in_flight_file = (fun ~channel:_ ~file_id -> List.mem file_id in_flight);
    unconfirmed_since = (fun ~channel:_ -> since);
  }

let silent ?(registry = G.no_relays) ?(root = Some (F.own_relay ())) text =
  G.silent ~registry ~bot:"UBOT" ~channel:"C0TEST" ~root_ts:"1700004000.000500"
    ~root ~text

(* A registry whose lookups all raise, as a failing phase-3 DB would. *)
let raising =
  {
    G.recorded_relay = (fun ~channel:_ ~ts:_ -> failwith "recorded_relay");
    in_flight_file = (fun ~channel:_ ~file_id:_ -> failwith "in_flight_file");
    unconfirmed_since = (fun ~channel:_ -> failwith "unconfirmed_since");
  }

let test_footer () =
  let n = F.relay_nonce in
  Alcotest.(check (option string))
    "footer is the last non-empty line" (Some n)
    (G.nonce_of_text ("intro\n" ^ G.footer ~nonce:n ^ "\n  "));
  List.iter
    (fun text ->
      Alcotest.(check (option string))
        ("no nonce in " ^ String.escaped text)
        None (G.nonce_of_text text))
    [
      "clawq-relay/1 " ^ String.uppercase_ascii n;
      "clawq-relay/1 abc";
      G.footer ~nonce:n ^ "\nmore text";
      "";
    ];
  Alcotest.check_raises "invalid nonce refused"
    (Invalid_argument "Slack_relay_guard.footer") (fun () ->
      ignore (G.footer ~nonce:"bad"))

let test_silence () =
  let silent_when name v = check_bool name true v
  and turn_when name v = check_bool name false v in
  silent_when "plain reply" (silent "thanks");
  silent_when "mentioning Claude stays silent" (silent "<@U0CLAUDE> go on");
  silent_when "a typed name is not a mention" (silent "clawq, summarise");
  silent_when "subtype never read"
    (silent ~root:(Some (F.own_relay ~subtype:"file_share" ())) "x");
  silent_when "recorded relay without a root"
    (silent ~registry:(reg ~recorded:true ()) ~root:None "x");
  silent_when "in-flight file"
    (silent
       ~registry:(reg ~in_flight:[ "F9" ] ())
       ~root:
         (Some (F.own_relay ~footer:None ~files:[ F.md_file ~id:"F9" () ] ()))
       "x");
  silent_when "own file post after an unconfirmed upload started"
    (silent
       ~registry:(reg ~since:1700003000. ())
       ~root:(Some (F.own_relay ~footer:None ()))
       "x");
  turn_when "explicit mention" (silent "<@UBOT> summarise");
  turn_when "labelled mention" (silent "<@UBOT|clawq> hi");
  turn_when "human root" (silent ~root:(Some (F.human "hi")) "x");
  turn_when "a human-authored footer is no relay"
    (silent ~root:(Some (F.own_relay ~user:"U123" ())) "x");
  turn_when "no footer" (silent ~root:(Some (F.own_relay ~footer:None ())) "x");
  turn_when "no files" (silent ~root:(Some (F.own_relay ~files:[] ())) "x");
  turn_when "own file post before the upload started"
    (silent
       ~registry:(reg ~since:1700005000. ())
       ~root:(Some (F.own_relay ~footer:None ()))
       "x");
  turn_when "invalid root ts"
    (silent
       ~registry:(reg ~since:1700003000. ())
       ~root:(Some (F.own_relay ~footer:None ~ts:"bad" ()))
       "x");
  turn_when "the stub registry knows no relays" (silent ~root:None "x");
  turn_when "mention overrides a recorded relay"
    (silent ~registry:(reg ~recorded:true ()) ~root:None "<@UBOT> go");
  turn_when "mention overrides an in-flight file"
    (silent
       ~registry:(reg ~in_flight:[ "F0MD" ] ())
       ~root:(Some (F.own_relay ~footer:None ()))
       "<@UBOT> go");
  check_bool "own relay" true
    (G.is_own_relay_message ~bot:"UBOT" (F.own_relay ()));
  check_bool "own relay without bot_id" true
    (G.is_own_relay_message ~bot:"UBOT" (F.own_relay ~bot_id:None ()))

(* Makes [conversations.replies] answer [messages]; every other method goes to
   the handler installed before. *)
let with_replies handler messages =
  let old = !handler in
  handler :=
    fun method_ params ->
      if method_ = "conversations.replies" then
        ok [ ("messages", arr messages); ("has_more", `Bool false) ]
      else old method_ params

let relay_root handler =
  with_replies handler
    [
      F.own_relay ~channel:"C123" ~ts:"100.000001" ();
      message ~ts:"100.000002" "earlier reply";
    ]

let count name calls = List.length (List.filter (fun (m, _) -> m = name) !calls)

(* Each lookup that raises fails closed on its own; a human root makes the
   registry the only reason for silence. *)
let test_registry_fails_closed () =
  let silent_when name v = check_bool name true v in
  let human = Some (F.human "hi") in
  silent_when "raising registry, human root"
    (silent ~registry:raising ~root:human "x");
  silent_when "raising registry, unreadable thread"
    (silent ~registry:raising ~root:None "x");
  check_bool "mention still runs with a raising registry" false
    (silent ~registry:raising ~root:None "<@UBOT> x");
  silent_when "raising recorded_relay alone"
    (silent
       ~registry:
         {
           G.no_relays with
           recorded_relay = (fun ~channel:_ ~ts:_ -> failwith "db");
         }
       ~root:human "x");
  silent_when "raising in_flight_file alone"
    (silent
       ~registry:
         {
           G.no_relays with
           in_flight_file = (fun ~channel:_ ~file_id:_ -> failwith "db");
         }
       ~root:
         (Some (update (F.human "see file") [ ("files", arr [ F.md_file () ]) ]))
       "x");
  silent_when "raising unconfirmed_since alone"
    (silent
       ~registry:
         {
           G.no_relays with
           unconfirmed_since = (fun ~channel:_ -> failwith "db");
         }
       ~root:(Some (F.own_relay ~footer:None ()))
       "x");
  check_bool "history check fails closed" true
    (G.untrusted_in_history ~registry:raising ~channel:"C0TEST" (F.human "hi"))

let test_relay_reply () =
  with_ingress (fun _ calls handler seen _ run ->
      relay_root handler;
      let* ack =
        run (message ~thread:"100.000001" ~ts:"100.000004" "what now")
      in
      Alcotest.(check string) "acknowledged" "ok" ack;
      check_bool "no model turn" true (!seen = []);
      Alcotest.(check int) "no post" 0 (count "chat.postMessage" calls);
      Alcotest.(check int)
        "no status" 0
        (count "agents.sessions.setStatus" calls);
      Alcotest.(check int)
        "thread read once" 1
        (count "conversations.replies" calls);
      Lwt.return_unit)

let test_mention_runs () =
  with_ingress (fun _ _ handler seen _ run ->
      relay_root handler;
      let* _ =
        run (message ~thread:"100.000001" ~ts:"100.000004" "<@UBOT> summarise")
      in
      check_bool "explicit mention reaches the model" true (!seen <> []);
      Lwt.return_unit)

(* HTTP-level failure for conversations.replies only; the with_ingress fixture
   always answers 200, so wrap the transport. *)
let failing_replies ~hits ~status ~error f =
  match Lwt.get Slack_transport.key with
  | None -> Alcotest.fail "with_ingress transport missing"
  | Some outer ->
      let get ~uri ~headers =
        if
          Filename.basename (Uri.path (Uri.of_string uri))
          = "conversations.replies"
        then (
          incr hits;
          Lwt.return
            ( status,
              Yojson.Safe.to_string
                (obj [ ("ok", `Bool false); ("error", str error) ]) ))
        else outer.Slack_transport.get ~uri ~headers
      in
      Slack_transport.with_http { outer with Slack_transport.get } f

let with_source source f =
  let old = !G.registry_source in
  G.registry_source := source;
  Lwt.finalize f (fun () ->
      G.registry_source := old;
      Lwt.return_unit)

let with_registry registry f = with_source (fun _ -> registry) f

let test_replies_failure () =
  with_ingress (fun _ _ _ seen _ run ->
      let hits = ref 0 in
      let reply id ts =
        run ~id (message ~thread:"100.000001" ~ts "still there?")
      in
      (* Each event must hit the injected failure, or the default human root
         would keep every assertion true. *)
      let injected label =
        let before = !hits in
        fun () -> check_bool (label ^ ": failure injected") true (!hits > before)
      in
      let* () =
        failing_replies ~hits ~status:500 ~error:"internal_error" (fun () ->
            let ev1 = injected "Ev1" in
            let* _ =
              with_registry (reg ~recorded:true ()) (fun () ->
                  reply "Ev1" "100.000004")
            in
            ev1 ();
            check_bool "HTTP 500: recorded relay stays silent" true (!seen = []);
            let ev2 = injected "Ev2" in
            let* _ = reply "Ev2" "100.000005" in
            ev2 ();
            check_bool "HTTP 500: unknown root gets a normal turn" true
              (!seen <> []);
            Lwt.return_unit)
      in
      seen := [];
      failing_replies ~hits ~status:200 ~error:"not_in_channel" (fun () ->
          let ev3 = injected "Ev3" in
          let* _ =
            with_registry (reg ~recorded:true ()) (fun () ->
                reply "Ev3" "100.000006")
          in
          ev3 ();
          check_bool "not_in_channel: recorded relay stays silent" true
            (!seen = []);
          Lwt.return_unit))

(* A raising registry (phase 3's DB) never escapes ingress: the reply gets no
   turn, the event is acknowledged, and a mention still runs. *)
let test_raising_registry_ingress () =
  with_ingress (fun _ calls _ seen _ run ->
      let* ack =
        with_registry raising (fun () ->
            run (message ~thread:"100.000001" ~ts:"100.000004" "still there?"))
      in
      Alcotest.(check string) "raising callbacks: acknowledged" "ok" ack;
      check_bool "raising callbacks: no model turn" true (!seen = []);
      Alcotest.(check int)
        "raising callbacks: no post" 0
        (count "chat.postMessage" calls);
      let* ack =
        with_source
          (fun _ -> failwith "source")
          (fun () ->
            run ~id:"Ev2"
              (message ~thread:"100.000001" ~ts:"100.000005" "still there?"))
      in
      Alcotest.(check string) "raising source: acknowledged" "ok" ack;
      check_bool "raising source: no model turn" true (!seen = []);
      let* _ =
        with_registry raising (fun () ->
            run ~id:"Ev3"
              (message ~thread:"100.000001" ~ts:"100.000006" "<@UBOT> go on"))
      in
      check_bool "mention runs despite a raising registry" true (!seen <> []);
      Lwt.return_unit)

(* §7.12: a command reply fetches the thread only so the gate sees its root. *)
let test_relay_command_reply () =
  let reply = message ~thread:"100.000001" ~ts:"100.000004" "/help" in
  with_ingress (fun _ calls _ _ _ run ->
      let* _ = run reply in
      check_bool "control: /help posts in a human-rooted thread" true
        (count "chat.postMessage" calls >= 1);
      Lwt.return_unit);
  with_ingress (fun _ calls handler seen _ run ->
      relay_root handler;
      let* ack = run reply in
      Alcotest.(check string) "acknowledged" "ok" ack;
      check_bool "no model turn" true (!seen = []);
      Alcotest.(check int) "no post" 0 (count "chat.postMessage" calls);
      Alcotest.(check int)
        "no status" 0
        (count "agents.sessions.setStatus" calls);
      Alcotest.(check int)
        "thread read once" 1
        (count "conversations.replies" calls);
      Lwt.return_unit)

module S = Test_slack_standing_order_support
module Store = Slack_standing_order_store
module P = Test_slack_standing_order_pickup

let db_registry db : G.registry =
  {
    recorded_relay = Store.recorded_relay db;
    in_flight_file = Store.in_flight_file db;
    unconfirmed_since = Store.unconfirmed_since db;
  }

let test_retained_registry () =
  P.fixture (fun db _ o id lease ->
      P.second db id lease;
      let r = db_registry db in
      List.iter
        (fun ts ->
          check_bool "retained relay" true
            (r.recorded_relay ~channel:"C123" ~ts))
        [ P.ts0; P.ts1 ];
      List.iter
        (fun file_id ->
          check_bool "retained uploaded file" true
            (r.in_flight_file ~channel:"C123" ~file_id))
        [ "FONE"; "FTWO" ];
      check_bool "unrelated relay" false
        (r.recorded_relay ~channel:"C123" ~ts:"999.000001");
      check_bool "unrelated file" false
        (r.in_flight_file ~channel:"C123" ~file_id:"FUNKNOWN");
      check_bool "other channel" false
        (r.in_flight_file ~channel:"C999" ~file_id:"FONE");
      ignore
        (Test_slack_standing_order_fire_store.ok
           (Store.deactivate db ~order_id:o.id (Store.Cancel "U123") ~now:S.now));
      List.iter
        (fun ts ->
          check_bool "cancel preserves retained relay" true
            (r.recorded_relay ~channel:"C123" ~ts))
        [ P.ts0; P.ts1 ];
      List.iter
        (fun file_id ->
          check_bool "cancel preserves retained file" true
            (r.in_flight_file ~channel:"C123" ~file_id))
        [ "FONE"; "FTWO" ])

let test_unconfirmed_registry () =
  P.fixture (fun db _ o id lease ->
      P.second db id lease;
      Store.exec db
        "UPDATE slack_standing_order_dispatches SET \
         relay_ts=NULL,upload_started_at=? + attempt_no WHERE fire_id=?"
        [ Sqlite3.Data.FLOAT S.now; Store.text id ];
      let r = db_registry db in
      ignore
        (Test_slack_standing_order_fire_store.ok
           (Store.deactivate db ~order_id:o.id (Store.Cancel "U123") ~now:S.now));
      Alcotest.(check (option (float 0.)))
        "earliest unresolved attempt" (Some S.now)
        (r.unconfirmed_since ~channel:"C123");
      Store.exec db
        "UPDATE slack_standing_order_dispatches SET relay_ts=? WHERE fire_id=? \
         AND attempt_no=0"
        [ Store.text P.ts0; Store.text id ];
      Alcotest.(check (option (float 0.)))
        "second remains unresolved"
        (Some (S.now +. 1.))
        (r.unconfirmed_since ~channel:"C123");
      Store.exec db
        "UPDATE slack_standing_order_dispatches SET relay_ts=? WHERE fire_id=? \
         AND attempt_no=1"
        [ Store.text P.ts1; Store.text id ];
      Alcotest.(check (option (float 0.)))
        "all resolved" None
        (r.unconfirmed_since ~channel:"C123"))

let test_undispatched_registry ~cancel ~reason ~files () =
  with_ingress (fun base _ _ _ _ _ ->
      let o = S.seed base.db ~channel:"C0TEST" () in
      let fire =
        S.fire base.db o
          ~state:
            Slack_standing_order_types.(if cancel then Uploading else Failed)
          ~room:"R1" ()
      in
      Store.exec base.db
        "UPDATE slack_standing_order_fires SET reason=? WHERE id=?"
        [ Store.text reason; Store.text fire.id ];
      Store.save_attempt base.db fire.id
        {
          (Store.empty_attempt 0) with
          upload_started_at = Some S.now;
          uploaded_file_ids = files;
        };
      if cancel then
        ignore
          (Test_slack_standing_order_fire_store.ok
             (Store.deactivate base.db ~order_id:o.id (Store.Cancel "U123")
                ~now:S.now));
      let registry = G.phase1_registry base.manager in
      Alcotest.(check (option (float 0.)))
        "undispatched upload is not an unresolved post" None
        (registry.unconfirmed_since ~channel:"C0TEST");
      check_bool "unrelated own file thread remains conversational" false
        (silent ~registry ~root:(Some (F.own_relay ~footer:None ())) "hello");
      Lwt.return_unit)

let test_invalid_upload_time value () =
  with_ingress (fun base _ _ _ _ _ ->
      let o = S.seed base.db ~channel:"C0TEST" () in
      let fire =
        S.fire base.db o ~state:Slack_standing_order_types.Posting_unknown
          ~dispatched_at:S.now ~room:"R1" ()
      in
      Store.ensure_schema base.db;
      Store.exec base.db
        "UPDATE slack_standing_order_dispatches SET upload_started_at=? WHERE \
         fire_id=?"
        [ value; Store.text fire.id ];
      let registry = G.phase1_registry base.manager in
      check_bool "invalid upload time raises instead of appearing absent" true
        (match registry.unconfirmed_since ~channel:"C0TEST" with
        | _ -> false
        | exception Failure _ -> true);
      Alcotest.(check (option (float 0.)))
        "total registry supplies conservative upload time"
        (Some Float.neg_infinity)
        ((G.total registry).unconfirmed_since ~channel:"C0TEST");
      let root = Some (F.own_relay ~footer:None ()) in
      check_bool "unreadable upload time stays silent" true
        (silent ~registry ~root "hello");
      check_bool "explicit mention escapes unreadable upload time" false
        (silent ~registry ~root "<@UBOT> hello");
      check_bool "unreadable registry history remains untrusted" true
        (G.untrusted_in_history ~registry ~channel:"C0TEST" (F.human "hello"));
      Lwt.return_unit)

let test_db_unreadable () =
  with_ingress (fun base _ _ _ _ _ ->
      let o = S.seed base.db ~channel:"C0TEST" () in
      ignore
        (S.fire base.db o ~state:Slack_standing_order_types.Posted
           ~dispatched_at:S.now ~relay_ts:P.ts0 ~room:"R1" ());
      Store.ensure_schema base.db;
      Store.exec base.db
        "UPDATE slack_standing_order_dispatches SET \
         uploaded_file_ids='unreadable'"
        [];
      let registry = G.phase1_registry base.manager in
      check_bool "unreadable silent" true (silent ~registry ~root:None "hello");
      check_bool "explicit mention escapes" false
        (silent ~registry ~root:None "<@UBOT> hello");
      check_bool "history untrusted" true
        (G.untrusted_in_history ~registry ~channel:"C0TEST" (F.human "hello"));
      Lwt.return_unit)

let test_production_registry () =
  with_ingress (fun base _ _ _ _ _ ->
      let o = S.seed base.db () in
      ignore
        (S.fire base.db o ~state:Slack_standing_order_types.Posted
           ~dispatched_at:S.now ~relay_ts:P.ts0 ~room:"R1" ());
      let r = G.phase1_registry base.manager in
      check_bool "production body reads DB" true
        (r.recorded_relay ~channel:"C123" ~ts:P.ts0);
      Lwt.return_unit)

let test_no_db_registry () =
  with_ingress (fun base _ _ _ _ _ ->
      let manager = { base.manager with db = None } in
      let registry = G.phase1_registry manager in
      check_bool "no DB silent" true (silent ~registry ~root:None "hello");
      check_bool "no DB mention escapes" false
        (silent ~registry ~root:None "<@UBOT> hello");
      check_bool "no DB history untrusted" true
        (G.untrusted_in_history ~registry ~channel:"C123" (F.human "hello"));
      Lwt.return_unit)

let test_null_subtype () =
  check_bool "null subtype relay is silent" true
    (silent
       ~root:(Some (update (F.own_relay ()) [ ("subtype", `Null) ]))
       "hello")

let test_mention_escape () =
  check_bool "explicit mention escapes relay silence" false
    (silent "<@UBOT> hello")

let suite =
  [
    S.case "registry retains both attempt relays and file sets"
      test_retained_registry;
    S.case "registry protects earliest unresolved upload"
      test_unconfirmed_registry;
    S.case "allocation failure does not silence unrelated threads"
      (test_undispatched_registry ~cancel:false
         ~reason:"upload_allocation_failed" ~files:[]);
    S.case "raw upload failure does not silence unrelated threads"
      (test_undispatched_registry ~cancel:false ~reason:"upload_failed"
         ~files:[ "FUPLOAD" ]);
    S.case "cancel before dispatch does not silence unrelated threads"
      (test_undispatched_registry ~cancel:true ~reason:"cancelled"
         ~files:[ "FUPLOAD" ]);
    S.case "malformed upload time fails closed"
      (test_invalid_upload_time (Sqlite3.Data.TEXT "invalid_time"));
    S.case "infinite upload time fails closed"
      (test_invalid_upload_time (Sqlite3.Data.FLOAT Float.infinity));
    S.case "negative infinite upload time fails closed"
      (test_invalid_upload_time (Sqlite3.Data.FLOAT Float.neg_infinity));
    S.case "registry unreadable fails closed" test_db_unreadable;
    S.case "production registry reads stored relays" test_production_registry;
    S.case "registry without database fails closed" test_no_db_registry;
    S.case "registry accepts null subtype" test_null_subtype;
    S.case "explicit Clawq mention escapes relay silence" test_mention_escape;
    ("relay footer format", `Quick, test_footer);
    ("silence predicate", `Quick, test_silence);
    ("relay thread reply starts no turn", `Quick, test_relay_reply);
    ("explicit mention in relay thread runs", `Quick, test_mention_runs);
    ("replies failure falls back to registry", `Quick, test_replies_failure);
    ("raising registry fails closed", `Quick, test_registry_fails_closed);
    ( "raising registry never escapes ingress",
      `Quick,
      test_raising_registry_ingress );
    ("command reply in relay thread is silent", `Quick, test_relay_command_reply);
  ]
