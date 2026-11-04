(* Bound card clicks (spec §6.8, §7.1, §7.5): Confirm, Cancel, Resume and
   Pause through real [block_actions] ingress, and the activation notice. *)
open Lwt.Syntax
module F = Test_slack_standing_order_fixtures
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Render = Slack_standing_order_render
module G = Slack_standing_order_grant

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let check_opt = Alcotest.(check (option string))
let contains = Test_helpers.string_contains

let find (env : F.env) (o : T.order) =
  match Store.find env.db o.id with
  | Some o -> o
  | None -> Alcotest.fail ("no order " ^ o.id)

let check_state env o want =
  Alcotest.check S.state_t "order state" want (find env o).state

let has_text env text = List.mem text (F.texts env)
let mentions env needle = List.exists (fun t -> contains t needle) (F.texts env)

let count_texts env needle =
  List.length (List.filter (fun t -> contains t needle) (F.texts env))

(* Accepted posts whose params mention [needle] (an action id, say). *)
let posts_with (env : F.env) needle =
  List.filter
    (fun (params, _) -> contains (Yojson.Safe.to_string params) needle)
    !(env.posted)

let ledger (env : F.env) event_type =
  Room_activity_ledger.query ~db:env.db ~room_id:"slack:C123" ~event_type ()

let mark env = List.length (F.methods env)

(* The Slack methods called since [mark env] returned [m]. *)
let calls_since env m = List.filteri (fun i _ -> i >= m) (F.methods env)

let updates (env : F.env) =
  List.filter_map
    (fun (m, p) -> if m = "chat.update" then Some p else None)
    !(env.calls)

let order_arg n = `Assoc [ ("order", `String n) ]

let posted_ts (card : T.card) =
  match card.message_ts with
  | Some ts -> ts
  | None -> Alcotest.fail ("card " ^ card.id ^ " was never posted")

let press env ?user ?action_ts ?team ?app ?channel ?card_id ?message_ts ~action
    (card : T.card) =
  let message_ts =
    match message_ts with Some ts -> ts | None -> posted_ts card
  in
  let card_id = Option.value card_id ~default:card.id in
  let+ (_ : string) =
    F.click env ~action_id:action ~card_id ~message_ts ?user ?action_ts ?team
      ?app ?channel ()
  in
  ()

let confirm env ?user ?action_ts card =
  press env ?user ?action_ts ~action:Render.action_confirm card

let cancel env ?user card = press env ?user ~action:Render.action_cancel card

let resume env ?user ?action_ts card =
  press env ?user ?action_ts ~action:Render.action_resume card

let pause env ?user card = press env ?user ~action:Render.action_pause card

(* [F.create] by U123, then the pending order and its Confirm card. *)
let planned (env : F.env) =
  let* out = F.create env () in
  match Store.list env.db ~channel_id:"C123" () with
  | [ o ] -> Lwt.return (o, F.card_for env ~order_id:o.id ~kind:T.Confirm_card)
  | _ -> Alcotest.fail ("planned: no single pending order: " ^ out)

(* A confirmed SO-1 and its activation notice's Pause button. *)
let confirmed env =
  let* o, card = planned env in
  let* () = confirm env card in
  check_state env o T.Active;
  Lwt.return (find env o, F.card_for env ~order_id:o.id ~kind:T.Pause_button)

(* The resume card the tool posts for SO-1. *)
let resume_card ?user (env : F.env) (o : T.order) =
  let* out =
    F.invoke env ?user "slack_standing_order_resume" (order_arg "SO-1")
  in
  check_bool ("resume card posted: " ^ out) true (contains out "was posted");
  Lwt.return (F.card_for env ~order_id:o.id ~kind:T.Resume_card)

let make_guest = function
  | `Assoc xs ->
      `Assoc
        (("is_restricted", `Bool true) :: List.remove_assoc "is_restricted" xs)
  | json -> json

let deny_content cfg = S.profile cfg ~denied:[ "slack_huddle_content" ] ()

let resume_deny_text =
  "Not resumed: this room's profile does not allow slack_huddle_content."

(* ---- Confirm ---- *)

let test_card_actions () =
  List.iter
    (fun (id, want) ->
      check_bool id want (Slack_standing_order_cards.is_card_action id))
    [
      (Render.action_confirm, true);
      (Render.action_cancel, true);
      (Render.action_resume, true);
      (Render.action_pause, true);
      ("clawq_stop_run", false);
      ("clawq_so_other", false);
      ("", false);
    ]

let test_confirm_activates () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = confirm env card in
      let now = find env o in
      Alcotest.check S.state_t "active" T.Active now.state;
      check_opt "active_since is the click" (Some "1700000000.000200")
        now.active_since;
      check_opt "confirmed by the grantor" (Some "U123") now.confirmed_by;
      check_opt "bot user" (Some "UBOT") now.bot_user_id;
      check_opt "bot" (Some "BOWN") now.bot_id;
      (match posts_with env Render.action_pause with
      | [ (params, ts) ] ->
          check_string "top level" "" (S.param params "thread_ts");
          check_bool "no target mention" false
            (contains (Yojson.Safe.to_string params) "<@UCLAUDE>");
          check_bool "unfurls off" true
            (Slack_huddles.get "unfurl_links" params = `Bool false);
          let button = F.card_for env ~order_id:o.id ~kind:T.Pause_button in
          check_opt "pause button bound to the notice" (Some ts)
            button.message_ts;
          check_bool "pause button never expires" true (button.expires_at = None)
      | posts ->
          Alcotest.failf "expected one activation notice, got %d"
            (List.length posts));
      (match updates env with
      | [ p ] ->
          check_string "the card message" (posted_ts card) (S.param p "ts");
          check_string "closed"
            (Render.card_closed now (Render.Confirmed_by "U123")).text
            (S.param p "text")
      | ps -> Alcotest.failf "expected one chat.update, got %d" (List.length ps));
      check_bool "card used" true
        ((F.card_for env ~order_id:o.id ~kind:T.Confirm_card).used_at <> None);
      Alcotest.(check int)
        "confirmed in the ledger" 1
        (List.length (ledger env "standing_order_confirmed"));
      Lwt.return_unit)

let test_only_grantor_confirms () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = confirm env ~user:"U456" card in
      check_state env o T.Pending_confirmation;
      check_bool "refusal in the thread" true
        (List.exists
           (fun (p, _) ->
             S.param p "text" = "Only <@U123> can confirm this card."
             && S.param p "thread_ts" = F.thread_root)
           !(env.posted));
      Lwt.return_unit)

let test_binding () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let posts = List.length !(env.posted) and before = mark env in
      let attempts =
        [
          (fun () ->
            press env ~message_ts:"999.1" ~action:Render.action_confirm card);
          (fun () ->
            press env ~channel:"C999" ~action:Render.action_confirm card);
          (fun () -> press env ~team:"T999" ~action:Render.action_confirm card);
          (fun () -> press env ~app:"A999" ~action:Render.action_confirm card);
          (fun () ->
            press env ~card_id:"nope" ~action:Render.action_confirm card);
          (fun () -> press env ~user:"bob" ~action:Render.action_confirm card);
          (fun () -> resume env card);
          (fun () -> pause env card);
        ]
      in
      let* () = Lwt_list.iter_s (fun attempt -> attempt ()) attempts in
      check_state env o T.Pending_confirmation;
      Alcotest.(check int) "no new posts" posts (List.length !(env.posted));
      Alcotest.(check (list string)) "no Slack call" [] (calls_since env before);
      check_bool "card unused" true
        ((F.card_for env ~order_id:o.id ~kind:T.Confirm_card).used_at = None);
      Lwt.return_unit)

let test_confirm_action_ts () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () =
        Lwt_list.iter_s
          (fun action_ts -> confirm env ~action_ts card)
          [ None; Some ""; Some "abc" ]
      in
      check_state env o T.Pending_confirmation;
      check_opt "no active_since" None (find env o).active_since;
      Alcotest.(check int)
        "each click refused" 3
        (count_texts env
           "Slack did not include a click time; press Confirm again.");
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

let test_expired_card () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      S.at (S.now +. 601.);
      let* () = confirm env card in
      let now = find env o in
      Alcotest.check S.state_t "withdrawn" T.Cancelled now.state;
      check_opt "by expiry" (Some "expired") now.cancelled_by;
      check_bool "expired reply" true
        (has_text env "This card has expired; ask clawq again for a new one.");
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

let test_double_click () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = confirm env card in
      let* () = confirm env card in
      check_state env o T.Active;
      Alcotest.(check int)
        "one activation notice" 1
        (count_texts env "is active:");
      Alcotest.(check int) "one chat.update" 1 (List.length (updates env));
      Alcotest.(check int)
        "confirmed once" 1
        (List.length (ledger env "standing_order_confirmed"));
      Lwt.return_unit)

let test_cancel_after_confirm () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = confirm env card in
      let posts = List.length !(env.posted) and before = mark env in
      let* () = cancel env card in
      check_state env o T.Active;
      Alcotest.(check int) "no new post" posts (List.length !(env.posted));
      Alcotest.(check (list string)) "no Slack call" [] (calls_since env before);
      Lwt.return_unit)

let test_config_change () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      F.set_config env (fun c ->
          S.with_slack c (fun sc ->
              { sc with allow_private_channels = [ "G1" ] }));
      let* () = confirm env card in
      check_state env o T.Pending_confirmation;
      check_bool "re-run" true
        (has_text env
           "clawq's configuration changed since this card was made; ask again \
            to get a new card.");
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

let test_profile_deny_at_click () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      F.set_config env deny_content;
      let before = mark env in
      let* () = confirm env card in
      check_state env o T.Pending_confirmation;
      check_bool "refused" true
        (has_text env
           "Not confirmed: this room's profile does not allow \
            slack_huddle_content.");
      Alcotest.(check (list string))
        "only the refusal" [ "chat.postMessage" ] (calls_since env before);
      Lwt.return_unit)

let test_natural_language () =
  F.with_orders (fun env ->
      let* o, _ = planned env in
      let* _ = F.say env ~thread:F.thread_root "confirm" in
      let* _ = F.say env ~thread:F.thread_root "Confirm SO-1" in
      check_bool "the turns ran" true (!(env.seen) <> []);
      check_state env o T.Pending_confirmation;
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

(* A pending order withdrawn behind an unused card (a typed pause here; a
   failed plan in the create tool) never confirms through that card. *)
let test_withdrawn_never_confirms () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* _ = F.say env ~thread:F.thread_root "pause SO-1" in
      check_state env o T.Cancelled;
      let* () = confirm env card in
      check_state env o T.Cancelled;
      check_bool "expired reply" true
        (has_text env "This card has expired; ask clawq again for a new one.");
      let before = mark env in
      let* () = cancel env card in
      Alcotest.(check (list string))
        "a cancel makes no Slack call" [] (calls_since env before);
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

(* ---- Cancel ---- *)

let test_cancel_withdraws () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = cancel env card in
      let now = find env o in
      Alcotest.check S.state_t "withdrawn" T.Cancelled now.state;
      check_opt "by the grantor" (Some "U123") now.cancelled_by;
      (match updates env with
      | [ p ] ->
          check_string "the card message" (posted_ts card) (S.param p "ts");
          check_bool "withdrawn" true (contains (S.param p "text") "withdrawn")
      | ps -> Alcotest.failf "expected one chat.update, got %d" (List.length ps));
      Alcotest.(check int)
        "withdrawn in the ledger" 1
        (List.length (ledger env "standing_order_withdrawn"));
      let* () = confirm env card in
      check_state env o T.Cancelled;
      check_bool "no activation notice" true
        (posts_with env Render.action_pause = []);
      Lwt.return_unit)

let test_cancel_authority () =
  F.with_orders (fun env ->
      let* o, card = planned env in
      let* () = cancel env ~user:"U456" card in
      check_state env o T.Pending_confirmation;
      check_bool "not the grantor" true
        (has_text env "Only <@U123> or a clawq admin can cancel SO-1.");
      S.remove_member env.world "U123";
      let* () = cancel env card in
      check_state env o T.Pending_confirmation;
      check_bool "not a member" true
        (has_text env
           ("Not cancelled: "
           ^ Render.member_refusal_text ~action:`Cancel G.Member_not_member
           ^ "."));
      check_bool "no chat.update" true (updates env = []);
      env.world.members := "U123" :: !(env.world.members);
      F.admin env "U456";
      let* () = cancel env ~user:"U456" card in
      check_state env o T.Cancelled;
      check_opt "by the admin" (Some "U456") (find env o).cancelled_by;
      Lwt.return_unit)

(* ---- Resume ---- *)

let test_resume_click () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      let* () = resume env ~user:"U456" card in
      check_state env o T.Paused;
      check_bool "not the grantor" true
        (has_text env "Only <@U123> or a clawq admin can resume SO-1.");
      F.admin env "U456";
      let* card = resume_card env o in
      let before = find env o in
      let* () =
        resume env ~user:"U456" ~action_ts:(Some "1700000900.000000") card
      in
      let after = find env o in
      Alcotest.check S.state_t "active" T.Active after.state;
      check_opt "active_since is the click" (Some "1700000900.000000")
        after.active_since;
      Alcotest.(check int) "epoch bumped" (before.epoch + 1) after.epoch;
      check_bool "card closed" true
        (List.exists
           (fun p ->
             S.param p "text"
             = (Render.card_closed after (Render.Resumed_by "U456")).text)
           (updates env));
      check_bool "grantor told" true
        (has_text env
           (Render.grantor_notice after (Render.Resumed_by_user "U456")));
      Alcotest.(check int)
        "resumed in the ledger" 1
        (List.length (ledger env "standing_order_resumed"));
      let posts = List.length !(env.posted) and m = mark env in
      let* () =
        resume env ~user:"U456" ~action_ts:(Some "1700000950.000000") card
      in
      check_bool "the same card again changes nothing" true (find env o = after);
      Alcotest.(check int) "no new post" posts (List.length !(env.posted));
      Alcotest.(check (list string)) "no Slack call" [] (calls_since env m);
      Lwt.return_unit)

let test_resume_admin_allowlist () =
  F.with_orders
    ~config:(fun c -> S.with_allow_users c [ "U123" ])
    (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      F.admin env "U456";
      let* () = resume env ~user:"U456" card in
      check_state env o T.Paused;
      check_bool "the admin is not allowlisted" true
        (has_text env
           ("Not resumed: "
           ^ Render.member_refusal_text ~action:`Resume G.Member_not_allowed
           ^ "."));
      Lwt.return_unit)

let test_resume_action_ts () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      let epoch = (find env o).epoch in
      let* () =
        Lwt_list.iter_s
          (fun action_ts -> resume env ~action_ts card)
          [ None; Some ""; Some "abc" ]
      in
      check_state env o T.Paused;
      Alcotest.(check int) "same epoch" epoch (find env o).epoch;
      Alcotest.(check int)
        "each click refused" 3
        (List.length
           (List.filter
              (( = ) "Slack did not include a click time; press Resume again.")
              (F.texts env)));
      Lwt.return_unit)

let test_expired_resume_card () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      let epoch = (find env o).epoch in
      S.at (S.now +. 601.);
      let before = mark env in
      let* () = resume env card in
      check_state env o T.Paused;
      Alcotest.(check int) "same epoch" epoch (find env o).epoch;
      check_bool "expired" true
        (has_text env "This card has expired; ask clawq again for a new one.");
      check_bool "no channel read" true
        (List.for_all
           (fun m -> not (String.starts_with ~prefix:"conversations." m))
           (calls_since env before));
      Lwt.return_unit)

let test_resume_capability_deny () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      F.set_config env deny_content;
      let before = mark env in
      let* () = resume env card in
      check_state env o T.Paused;
      check_bool "grantor refused" true (has_text env resume_deny_text);
      Alcotest.(check (list string))
        "only the refusal" [ "chat.postMessage" ] (calls_since env before);
      F.admin env "U456";
      let before = mark env in
      let* () = resume env ~user:"U456" card in
      check_state env o T.Paused;
      Alcotest.(check int)
        "admin refused" 2
        (List.length (List.filter (( = ) resume_deny_text) (F.texts env)));
      Alcotest.(check (list string))
        "admin: only the refusal" [ "chat.postMessage" ]
        (calls_since env before);
      Lwt.return_unit)

let test_stale_resume_card () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      let e = card.order_epoch in
      (match
         Store.resume env.db ~order_id:o.id ~expected_epoch:e
           ~active_since:"1700000000.000300" ~bot_user_id:"UBOT" ~bot_id:"BOWN"
           ~now:S.now
       with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "seed resume failed");
      (match
         Store.deactivate env.db ~order_id:o.id (Store.Pause "U123") ~now:S.now
       with
      | Ok _ -> ()
      | Error e -> Alcotest.fail e);
      Alcotest.(check int) "epoch e+2" (e + 2) (find env o).epoch;
      let* () = resume env card in
      check_state env o T.Paused;
      Alcotest.(check int) "epoch kept" (e + 2) (find env o).epoch;
      check_bool "out of date" true
        (has_text env
           "This resume card is out of date; ask clawq for a new one.");
      Lwt.return_unit)

let test_resume_preflight () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* card = resume_card env o in
      F.override env "conversations.info" (fun params ->
          S.ok
            [
              ( "channel",
                S.obj
                  [
                    ("id", S.str (S.param params "channel"));
                    ("is_member", `Bool true);
                    ("is_private", `Bool false);
                    ("is_shared", `Bool false);
                    ("is_ext_shared", `Bool true);
                    ("is_org_shared", `Bool false);
                    ("is_pending_ext_shared", `Bool false);
                  ] );
            ]);
      let* () = resume env card in
      check_state env o T.Paused;
      check_bool "shared" true
        (has_text env
           ("Not resumed: " ^ Render.reason_text G.Channel_shared ^ "."));
      Lwt.return_unit)

(* ---- Pause button ---- *)

let test_pause_any_member () =
  F.with_orders
    ~config:(fun c -> S.with_allow_users c [ "U123" ])
    (fun env ->
      let* o, button = confirmed env in
      let* () = pause env ~user:"U456" button in
      check_state env o T.Paused;
      check_opt "paused by the clicker" (Some "U456") (find env o).paused_by;
      check_bool "receipt in the notice's thread" true
        (List.exists
           (fun (p, _) ->
             contains (S.param p "text") "is paused (by <@U456>)"
             && S.param p "thread_ts" = posted_ts button)
           !(env.posted));
      check_bool "grantor told" true
        (has_text env
           (Render.grantor_notice (find env o) (Render.Paused_by "U456")));
      let* () = pause env ~user:"U456" button in
      check_bool "not single-use" true
        (has_text env "Standing order SO-1 is already paused.");
      Lwt.return_unit)

let test_unknown_count_receipt () =
  F.with_orders (fun env ->
      let* o, button = confirmed env in
      let fire = S.fire env.db o ~state:T.Posting ~room:"R1" () in
      Sqlite3.create_fun0 env.db "count" (fun () ->
          failwith "fixture count failure");
      let* () = pause env ~user:"U456" button in
      check_state env o T.Paused;
      check_bool "receipt in the notice thread" true
        (List.exists
           (fun (p, _) ->
             S.param p "text"
             = "Standing order SO-1 is paused (by <@U456>). Huddles that \
                become ready while it is paused will never be relayed, even \
                after a resume. Relays may already be in flight."
             && S.param p "thread_ts" = posted_ts button)
           !(env.posted));
      check_bool "repost forbidden" true
        (S.fire_flags env.db fire.id = (T.Posting, true, false));
      Lwt.return_unit)

let test_pause_refusal () =
  F.with_orders (fun env ->
      let* o, button = confirmed env in
      S.edit_user env.world "U456" make_guest;
      let* () = pause env ~user:"U456" button in
      check_state env o T.Active;
      check_bool "reason posted" true (mentions env "Pause not applied");
      Lwt.return_unit)

let test_pause_llm_down () =
  F.with_orders (fun env ->
      let* o, button = confirmed env in
      F.fail_provider env;
      let* () = pause env ~user:"U456" button in
      check_state env o T.Paused;
      check_bool "receipt" true (mentions env "is paused (by <@U456>)");
      check_bool "no provider call" true (!(env.seen) = []);
      Lwt.return_unit)

(* The button's order is known, so a store that can't be read gets the
   refusal, never "There is no standing order". The click is built directly:
   binding reads the order too, and an unbound click is ignored. *)
let test_pause_store_failure () =
  F.with_orders (fun env ->
      let* o, button = confirmed env in
      S.drop_order_column env.db;
      let before = List.length !(env.posted) in
      let* () =
        Slack_standing_order_cards.pause
          {
            Slack_standing_order_cards.manager = env.base.manager;
            db = env.db;
            now = S.now;
            card = button;
            order = o;
            clicker = "U456";
            message_ts = posted_ts button;
            action_ts = None;
          }
      in
      Alcotest.(check (list string))
        "one refusal receipt"
        [
          Slack_standing_order_pause.receipt ~display_no:1 ~by:"U456"
            (Slack_standing_order_pause.Refused
               (G.Member_indeterminate "standing_order_store"));
        ]
        (List.filteri (fun i _ -> i >= before) (F.texts env));
      check_bool "never not-found" false
        (mentions env "There is no standing order");
      Lwt.return_unit)

(* ---- Activation notice ---- *)

let test_notice_failure () =
  F.with_orders (fun env ->
      F.override env "chat.postMessage" (fun params ->
          if S.param params "thread_ts" = "" then
            S.obj [ ("ok", `Bool false); ("error", S.str "channel_not_found") ]
          else S.default_response env.world "chat.postMessage" params);
      let* o, card = planned env in
      let* () = confirm env card in
      let now = find env o in
      Alcotest.check S.state_t "paused" T.Paused now.state;
      check_opt "by the system" (Some "system:activation_notice_failed")
        now.paused_by;
      check_bool "the grantor is told in the management thread" true
        (List.exists
           (fun (p, _) ->
             contains (S.param p "text") "could not post the channel notice"
             && S.param p "thread_ts" = F.thread_root)
           !(env.posted));
      Alcotest.(check int)
        "the confirm stands" 1
        (List.length (ledger env "standing_order_confirmed"));
      check_opt "the pause button is unbound" None
        (F.card_for env ~order_id:o.id ~kind:T.Pause_button).message_ts;
      Lwt.return_unit)

let suite =
  List.map
    (fun (name, f) -> (name, `Quick, f))
    [
      ("card actions are recognised", test_card_actions);
      ("confirm activates", test_confirm_activates);
      ("only the grantor confirms", test_only_grantor_confirms);
      ("binding", test_binding);
      ("confirm needs a valid action_ts", test_confirm_action_ts);
      ("expired card", test_expired_card);
      ("double click confirms once", test_double_click);
      ("cancel after confirm changes nothing", test_cancel_after_confirm);
      ("config change rejects", test_config_change);
      ("profile deny at click", test_profile_deny_at_click);
      ("natural language never confirms", test_natural_language);
      ("a withdrawn order never confirms", test_withdrawn_never_confirms);
      ("cancel button withdraws", test_cancel_withdraws);
      ("cancel click authority and membership", test_cancel_authority);
      ("resume click", test_resume_click);
      ("an admin resume needs the allowlist", test_resume_admin_allowlist);
      ("resume needs a valid action_ts", test_resume_action_ts);
      ("expired resume card", test_expired_resume_card);
      ("resume capability deny", test_resume_capability_deny);
      ("stale resume card", test_stale_resume_card);
      ("resume reruns the preflight", test_resume_preflight);
      ("pause button any member", test_pause_any_member);
      ("unknown count receipt", test_unknown_count_receipt);
      ("pause button refusal", test_pause_refusal);
      ("pause button with the llm down", test_pause_llm_down);
      ("pause button store failure refuses", test_pause_store_failure);
      ("activation notice failure pauses the order", test_notice_failure);
    ]
