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
let failing_replies ~status ~error f =
  match Lwt.get Slack_transport.key with
  | None -> Alcotest.fail "with_ingress transport missing"
  | Some outer ->
      let get ~uri ~headers =
        if
          Filename.basename (Uri.path (Uri.of_string uri))
          = "conversations.replies"
        then
          Lwt.return
            ( status,
              Yojson.Safe.to_string
                (obj [ ("ok", `Bool false); ("error", str error) ]) )
        else outer.Slack_transport.get ~uri ~headers
      in
      Slack_transport.with_http { outer with Slack_transport.get } f

let with_registry registry f =
  let old = !G.registry_source in
  (G.registry_source := fun _ -> registry);
  Lwt.finalize f (fun () ->
      G.registry_source := old;
      Lwt.return_unit)

let test_replies_failure () =
  with_ingress (fun _ _ _ seen _ run ->
      let reply id ts =
        run ~id (message ~thread:"100.000001" ~ts "still there?")
      in
      let* () =
        failing_replies ~status:500 ~error:"internal_error" (fun () ->
            let* _ =
              with_registry (reg ~recorded:true ()) (fun () ->
                  reply "Ev1" "100.000004")
            in
            check_bool "HTTP 500: recorded relay stays silent" true (!seen = []);
            let* _ = reply "Ev2" "100.000005" in
            check_bool "HTTP 500: unknown root gets a normal turn" true
              (!seen <> []);
            Lwt.return_unit)
      in
      seen := [];
      failing_replies ~status:200 ~error:"not_in_channel" (fun () ->
          let* _ =
            with_registry (reg ~recorded:true ()) (fun () ->
                reply "Ev3" "100.000006")
          in
          check_bool "not_in_channel: recorded relay stays silent" true
            (!seen = []);
          Lwt.return_unit))

let suite =
  [
    ("relay footer format", `Quick, test_footer);
    ("silence predicate", `Quick, test_silence);
    ("relay thread reply starts no turn", `Quick, test_relay_reply);
    ("explicit mention in relay thread runs", `Quick, test_mention_runs);
    ("replies failure falls back to registry", `Quick, test_replies_failure);
  ]
