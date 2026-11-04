(* Fail-safe pause (spec §6.8, §7.5): the exact-match parser, the pause core
   through real ingress, receipts and the Decision 20 silence. *)
open Lwt.Syntax
module P = Slack_standing_order_pause
module F = Test_slack_standing_order_fixtures
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Render = Slack_standing_order_render

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let contains = Test_helpers.string_contains

let find (env : F.env) (o : T.order) =
  match Store.find env.db o.id with
  | Some o -> o
  | None -> Alcotest.fail ("no order " ^ o.id)

let check_state env o want =
  Alcotest.check S.state_t "order state" want (find env o).state

let has_text env text = List.mem text (F.texts env)
let mentions env needle = List.exists (fun t -> contains t needle) (F.texts env)
let called env method_ = List.mem method_ (F.methods env)

let count env method_ =
  List.length (List.filter (( = ) method_) (F.methods env))

let posts_with env pred =
  List.filter (fun (params, _) -> pred params) !(env.F.posted)

let ledger (env : F.env) event_type =
  Room_activity_ledger.query ~db:env.db ~room_id:"slack:C123" ~event_type ()

let paused_receipt ~by =
  Printf.sprintf
    "Standing order SO-1 is paused (by <@%s>). Huddles that become ready while \
     it is paused will never be relayed, even after a resume."
    by

let test_parse () =
  List.iter
    (fun (s, want) -> Alcotest.(check (option int)) s want (P.parse_command s))
    [
      ("pause SO-3", Some 3);
      ("PAUSE so-03", Some 3);
      ("  pause   SO-3.", Some 3);
      ("<@UBOT> pause SO-3", Some 3);
      ("pause SO-3 please", None);
      ("please pause SO-3", None);
      ("pause SO-0", None);
      ("pause SO-1234567", None);
      ("Anyone can say 'pause SO-3' or press Pause.", None);
    ]

let test_receipt_copy () =
  let o = { (S.seed (S.fresh_db ()) ()) with display_no = 4 } in
  let receipt outcome = P.receipt ~display_no:4 ~by:"U456" outcome in
  List.iter
    (fun (name, outcome, want) -> check_string name want (receipt outcome))
    [
      ( "several in flight",
        P.Paused { order = o; in_flight = 2 },
        "Standing order SO-4 is paused (by <@U456>). Huddles that become ready \
         while it is paused will never be relayed, even after a resume. 2 \
         relays were already being sent before the pause." );
      ( "already paused",
        P.Already_paused o,
        "Standing order SO-4 is already paused." );
      ( "withdrawn",
        P.Withdrawn o,
        "Standing order SO-4 was waiting for confirmation; it has been \
         withdrawn (by <@U456>)." );
      ( "suspended",
        P.Not_active { o with state = T.Suspended },
        "Standing order SO-4 is suspended and is not relaying; nothing changed."
      );
      ( "cancelled",
        P.Not_active { o with state = T.Cancelled },
        "Standing order SO-4 was cancelled; nothing to pause." );
      ("not found", P.Not_found 9, Render.not_found 9);
      ( "refused",
        P.Refused Slack_standing_order_grant.Member_not_member,
        "Pause not applied: only members of this channel can pause its \
         standing orders." );
    ];
  check_string "a non-user actor is verbatim"
    "Standing order SO-4 is paused (by cli). Huddles that become ready while \
     it is paused will never be relayed, even after a resume."
    (P.receipt ~display_no:4 ~by:"cli" (P.Paused { order = o; in_flight = 0 }))

let test_channel_has_orders () =
  let db = S.fresh_db () in
  let has ?(team = "T123") ?(app = "A123") channel =
    Store.channel_has_orders db ~team_id:team ~api_app_id:app
      ~channel_id:channel
  in
  check_bool "empty store" false (has "C123");
  let o = S.seed db ~state:T.Cancelled () in
  check_bool "a cancelled row counts" true (has "C123");
  check_bool "another channel" false (has "C999");
  check_bool "another app" false (has ~app:"A999" "C123");
  check_bool "another team" false (has ~team:"T999" "C123");
  Sql_util.exec_with_params db
    "UPDATE slack_standing_orders SET action_json = '{}' WHERE id = ?"
    [ Sqlite3.Data.TEXT o.id ];
  check_bool "the row no longer decodes" true (Store.find db o.id = None);
  check_bool "an undecodable row counts" true (has "C123")

let test_member_outside_allowlist () =
  F.with_orders
    ~config:(fun c -> S.with_allow_users c [ "U123" ])
    (fun env ->
      let o = F.seed env () in
      let* ack = F.say env ~user:"U456" "pause SO-1" in
      check_string "acknowledged" "ok" ack;
      check_state env o T.Paused;
      check_bool "receipt" true (has_text env (paused_receipt ~by:"U456"));
      check_bool "no provider turn" true (!(env.seen) = []);
      Lwt.return_unit)

let test_llm_down () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.fail_provider env;
      let* _ = F.say env "pause SO-1" in
      check_state env o T.Paused;
      check_bool "receipt" true (has_text env (paused_receipt ~by:"U123"));
      check_bool "no provider call" true (!(env.seen) = []);
      Lwt.return_unit)

let test_before_history () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* _ = F.say env ~thread:"150.000001" "pause SO-1" in
      check_state env o T.Paused;
      check_bool "no history read" false (called env "conversations.replies");
      check_bool "no chat authorization" false (called env "auth.test");
      check_bool "receipt in the thread" true
        (posts_with env (fun p -> S.param p "thread_ts" = "150.000001") <> []);
      Lwt.return_unit)

let without_subtype = function
  | `Assoc fields -> `Assoc (List.remove_assoc "subtype" fields)
  | json -> json

let test_relay_thread () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let relay =
        without_subtype
          (Test_slack_event_fixtures.own_relay ~channel:"C123" ~ts:"160.000001"
             ())
      in
      F.override env "conversations.replies" (fun _ ->
          S.ok [ ("messages", `List [ relay ]); ("has_more", `Bool false) ]);
      let* _ = F.say env ~thread:"160.000001" "pause SO-1" in
      check_state env o T.Paused;
      check_bool "receipt in the relay thread" true
        (posts_with env (fun p ->
             S.param p "thread_ts" = "160.000001"
             && S.param p "text" = paused_receipt ~by:"U123")
        <> []);
      let* _ = F.say env ~thread:"160.000001" "what do you think?" in
      check_bool "the relay thread was read for the gate" true
        (called env "conversations.replies");
      check_bool "the silence gate holds" true (!(env.seen) = []);
      Lwt.return_unit)

let test_thread_broadcast () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* _ =
        F.say env ~thread:F.thread_root ~subtype:"thread_broadcast" "pause SO-1"
      in
      check_state env o T.Paused;
      check_bool "receipt in the thread" true
        (posts_with env (fun p ->
             S.param p "thread_ts" = F.thread_root
             && S.param p "text" = paused_receipt ~by:"U123")
        <> []);
      let paused = find env o and posts = count env "chat.postMessage" in
      let* ack =
        F.say env ~thread:F.thread_root ~subtype:"thread_broadcast"
          "please help"
      in
      check_string "acknowledged" "ok" ack;
      check_bool "no provider call" true (!(env.seen) = []);
      Alcotest.(check int) "no post" posts (count env "chat.postMessage");
      check_bool "order unchanged" true (find env o = paused);
      Lwt.return_unit)

(* A human "also send to channel" reply that mentions clawq can arrive as both
   a [thread_broadcast] message and an [app_mention], in either order. *)
let broadcast env ~ts text =
  F.say env ~thread:F.thread_root ~ts ~subtype:"thread_broadcast" text

let mention (env : F.env) ~ts text =
  env.run ("mention" ^ ts)
    (Test_slack_threads.message ~kind:"app_mention" ~thread:F.thread_root ~ts
       text)

let test_broadcast_keeps_mention_turn () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let text = "<@UBOT> can you summarise?" in
      let turns () = List.length !(env.seen) in
      let* _ = broadcast env ~ts:"300.900001" text in
      Alcotest.(check int) "the broadcast starts no turn" 0 (turns ());
      let* _ = mention env ~ts:"300.900001" text in
      Alcotest.(check int)
        "a mention after the broadcast runs a turn" 1 (turns ());
      let* _ = mention env ~ts:"300.900002" text in
      let* _ = broadcast env ~ts:"300.900002" text in
      Alcotest.(check int)
        "a mention before the broadcast runs one turn" 2 (turns ());
      check_state env o T.Active;
      Lwt.return_unit)

let test_broadcast_pause_pair () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let text = "<@UBOT> pause SO-1" in
      let* _ = broadcast env ~ts:"300.900003" text in
      let* _ = mention env ~ts:"300.900003" text in
      check_state env o T.Paused;
      Alcotest.(check (list string))
        "broadcast first: one receipt"
        [ paused_receipt ~by:"U123" ]
        (F.texts env);
      let* _ = mention env ~ts:"300.900004" text in
      let* _ = broadcast env ~ts:"300.900004" text in
      Alcotest.(check (list string))
        "mention first: one more receipt"
        [ paused_receipt ~by:"U123"; "Standing order SO-1 is already paused." ]
        (F.texts env);
      check_bool "no provider turn" true (!(env.seen) = []);
      Lwt.return_unit)

let test_slackbot () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* _ = F.say env ~user:"USLACKBOT" ~thread:F.thread_root "pause SO-1" in
      check_state env o T.Active;
      Alcotest.(check (list string)) "no post" [] (F.texts env);
      check_bool "no users.info" false (called env "users.info");
      Lwt.return_unit)

let test_guest_refused () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      S.edit_user env.world "U456" (fun u ->
          Test_slack_huddle_fixtures.update u [ ("is_restricted", `Bool true) ]);
      let* _ = F.say env ~user:"U456" "pause SO-1" in
      check_state env o T.Active;
      check_bool "refusal receipt" true
        (has_text env
           "Pause not applied: guest accounts can't pause standing orders; ask \
            a full member of this channel.");
      check_bool "refusal recorded" true
        (ledger env "standing_order_pause_refused" <> []);
      Lwt.return_unit)

let mask_digits = String.map (function '0' .. '9' -> '#' | c -> c)

let test_not_found_identical () =
  F.with_orders (fun env ->
      let here = F.seed env () in
      (* One live order per channel and target: C999's SO-1 is cancelled, so
         its SO-2 can be active. *)
      let first = F.seed env ~channel:"C999" ~state:T.Cancelled () in
      let second = F.seed env ~channel:"C999" () in
      check_bool "C999 has an active SO-2" true
        (second.display_no = 2 && second.state = T.Active);
      let others = [ first; second ] in
      let snapshot () = List.map (find env) others in
      let before = snapshot () in
      let* _ = F.say env "pause SO-2" in
      let* _ = F.say env "pause SO-7" in
      Alcotest.(check (list string))
        "not-found receipts"
        [ Render.not_found 2; Render.not_found 7 ]
        (F.texts env);
      check_string "identical modulo the digit"
        (mask_digits (Render.not_found 2))
        (mask_digits (Render.not_found 7));
      check_bool "no member check" true
        (not (called env "users.info" || called env "conversations.members"));
      check_bool "C999 orders unchanged" true (snapshot () = before);
      check_state env here T.Active;
      Lwt.return_unit)

let private_channel channel =
  S.ok
    [
      ( "channel",
        S.obj
          [
            ("id", S.str channel);
            ("is_member", `Bool true);
            ("is_private", `Bool true);
            ("is_shared", `Bool false);
            ("is_ext_shared", `Bool false);
            ("is_org_shared", `Bool false);
            ("is_pending_ext_shared", `Bool false);
          ] );
    ]

let test_no_order_channel_silent () =
  F.with_orders
    ~config:(fun c ->
      S.with_slack c (fun sc ->
          {
            sc with
            private_channel_policy = Runtime_config.Pc_deny;
            allow_private_channels = [];
          }))
    (fun env ->
      F.override env "conversations.info" (fun params ->
          if S.param params "channel" = "C777" then private_channel "C777"
          else S.default_response env.world "conversations.info" params);
      let* ack, logs =
        S.with_log_capture (fun () -> F.say env ~channel:"C777" "pause SO-1")
      in
      check_string "acknowledged" "ok" ack;
      check_bool "no post" false (called env "chat.postMessage");
      check_bool "no users.info" false (called env "users.info");
      check_bool "consumed, no turn" true (!(env.seen) = []);
      check_bool "logged" true
        (List.exists
           (fun (level, msg) ->
             level = Logs.Info && contains msg "not answered")
           logs);
      check_bool "the log carries no user text" false
        (List.exists (fun (_, msg) -> contains msg "pause SO-1") logs);
      Lwt.return_unit)

let test_grantor_notified () =
  F.with_orders (fun env ->
      let _ = F.seed env () in
      let* _ = F.say env ~user:"U456" "pause SO-1" in
      check_bool "grantor notice in the management thread" true
        (posts_with env (fun p ->
             S.param p "thread_ts" = "100.000001"
             && S.param p "text"
                = "<@U123> Standing order SO-1 was paused by <@U456>.")
        <> []);
      match ledger env "standing_order_paused" with
      | [ event ] ->
          check_string "actor" "U456" event.actor;
          check_bool "metadata" true
            (event.metadata
            = `Assoc [ ("order", `String "SO-1"); ("source", `String "text") ]);
          Lwt.return_unit
      | events ->
          Alcotest.failf "expected one standing_order_paused, got %d"
            (List.length events))

let test_in_flight () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let _ = S.fire env.db o ~state:T.Posting ~room:"R1" () in
      let* _ = F.say env "pause SO-1" in
      check_state env o T.Paused;
      check_bool "relay disclosed" true
        (mentions env "1 relay was already being sent before the pause.");
      Lwt.return_unit)

let test_notice_failure () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.override env "chat.postMessage" (fun _ ->
          S.obj [ ("ok", `Bool false); ("error", S.str "channel_not_found") ]);
      let* _ = F.say env ~user:"U456" "pause SO-1" in
      check_state env o T.Paused;
      Alcotest.(check int)
        "receipt and grantor notice were both tried" 2
        (count env "chat.postMessage");
      Alcotest.(check (list string)) "nothing landed" [] (F.texts env);
      Lwt.return_unit)

let test_get_only_egress () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.set_config env (fun _ ->
          S.config ~strict_rules:[ S.slack_rule ~method_:"GET" "/api/*" ] ());
      let* _, logs =
        S.with_log_capture (fun () -> F.say env ~user:"U456" "pause SO-1")
      in
      check_state env o T.Paused;
      check_bool "member check ran over GET" true
        (called env "users.info" && called env "conversations.members");
      check_bool "no post reaches HTTP" false (called env "chat.postMessage");
      let denied unit_label =
        List.exists
          (fun (level, msg) ->
            level = Logs.Warning && contains msg unit_label
            && contains msg "egress_denied")
          logs
      in
      check_bool "receipt failure logged" true (denied "pause_receipt");
      check_bool "notice failure logged" true (denied "pause_notice");
      Lwt.return_unit)

let test_lookalikes () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* consumed =
        P.handle_text ~manager:env.base.manager
          ~route:
            {
              team = "T123";
              app = "A123";
              channel = "D123";
              root = "100.000001";
            }
          ~user:"U123" ~ts:"100.000001" "pause SO-1"
      in
      check_bool "a DM is not consumed" false consumed;
      Alcotest.(check (list string)) "no calls" [] (F.methods env);
      let* _ = F.say env "please pause SO-1" in
      check_bool "a normal provider turn" true (!(env.seen) <> []);
      check_state env o T.Active;
      Lwt.return_unit)

let test_pending_withdrawn () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Pending_confirmation () in
      let* _ = F.say env "pause SO-1" in
      let after = find env o in
      Alcotest.check S.state_t "withdrawn" T.Cancelled after.state;
      Alcotest.(check (option string))
        "cancelled_by" (Some "U123") after.cancelled_by;
      check_bool "receipt" true
        (has_text env
           "Standing order SO-1 was waiting for confirmation; it has been \
            withdrawn (by <@U123>).");
      check_bool "withdrawal recorded" true
        (ledger env "standing_order_withdrawn" <> []);
      (* Another member's withdrawal tells the grantor it was cancelled. *)
      let other = F.seed env ~channel:"C999" ~state:T.Pending_confirmation () in
      let* _ = F.say env ~user:"U456" ~channel:"C999" "pause SO-1" in
      check_state env other T.Cancelled;
      check_bool "grantor told it was cancelled" true
        (has_text env "<@U123> Standing order SO-1 was cancelled by <@U456>.");
      Lwt.return_unit)

let case name f = (name, `Quick, f)

let suite =
  [
    case "parse exact pause commands" test_parse;
    case "receipt copy" test_receipt_copy;
    case "channel_has_orders sees any row" test_channel_has_orders;
    case "member outside the allowlist can pause" test_member_outside_allowlist;
    case "pause works with the llm down" test_llm_down;
    case "pause runs before history" test_before_history;
    case "relay thread pause" test_relay_thread;
    case "thread broadcast pause" test_thread_broadcast;
    case "broadcast leaves the mention turn" test_broadcast_keeps_mention_turn;
    case "broadcast pause pair runs once" test_broadcast_pause_pair;
    case "slackbot text never pauses" test_slackbot;
    case "guest refused" test_guest_refused;
    case "not found is identical" test_not_found_identical;
    case "no reply where no order exists" test_no_order_channel_silent;
    case "grantor notified" test_grantor_notified;
    case "in-flight relay disclosed" test_in_flight;
    case "notice failure never undoes a pause" test_notice_failure;
    case "get-only egress still pauses" test_get_only_egress;
    case "DMs and lookalikes fall through" test_lookalikes;
    case "pending order is withdrawn" test_pending_withdrawn;
  ]
