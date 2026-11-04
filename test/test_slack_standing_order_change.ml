(* Change instruction (spec §6.8, Decision 14): the change tool's bound card,
   the atomic cancel-and-create click, Cancel on a change card, the change
   card's copy and [Store.replace_in_tx]'s rollback. Fake ids only. *)
open Lwt.Syntax
module F = Test_slack_standing_order_fixtures
module S = Test_slack_standing_order_support
module C = Test_slack_standing_order_cards
module RT = Test_slack_standing_order_render
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Render = Slack_standing_order_render
module Change = Slack_standing_order_change

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_opt = Alcotest.(check (option string))
let contains = Test_helpers.string_contains

let change_args ?(order = "SO-1") ?(instruction = "New text") ?when_no () :
    Yojson.Safe.t =
  `Assoc
    ([ ("order", `String order); ("instruction", `String instruction) ]
    @
    match when_no with
    | Some w -> [ ("when_no_transcript", `String w) ]
    | None -> [])

let change env ?user ?order ?instruction ?when_no () =
  F.invoke env ?user Change.tool_name
    (change_args ?order ?instruction ?when_no ())

let change_card env (o : T.order) =
  F.card_for env ~order_id:o.id ~kind:T.Change_card

(* [change] by [user], then the change card it posted. *)
let planned ?user ?when_no env (o : T.order) =
  let* out = change env ?user ?when_no () in
  check_bool ("change card posted: " ^ out) true (contains out "was posted");
  Lwt.return (change_card env o)

let confirm_change env ?user ?action_ts card =
  C.press env ?user ?action_ts ~action:Render.action_change card

let cancel_change env ?user card =
  C.press env ?user ~action:Render.action_cancel card

let all_orders (env : F.env) = Store.list env.db ~include_cancelled:true ()

let successor (env : F.env) =
  Store.find_by_display env.db ~team_id:"T123" ~channel_id:"C123" 2

let no_successor env =
  check_bool "no SO-2" true (successor env = None);
  check_int "one order" 1 (List.length (all_orders env))

(* The order, unchanged: same state and epoch as [o]. *)
let unchanged env (o : T.order) =
  let now = C.find env o in
  Alcotest.check S.state_t "state kept" o.state now.state;
  check_int "epoch kept" o.epoch now.epoch

let no_activation env =
  check_bool "no activation notice" true
    (C.posts_with env Render.action_pause = [])

let paused_line n =
  n
  ^ " is paused, so confirming starts relaying again. Huddles that became \
     ready while it was paused will not be relayed."

let suspended_line n =
  n
  ^ " is suspended, so confirming starts relaying again. Huddles that became \
     ready while it was suspended will not be relayed."

(* ---- The change card's copy ---- *)

let test_restart_disclosed () =
  let card state =
    Render.change_card ~replaces:(RT.order ~state ()) ~grantor:"U456"
      ~action:(S.relay_action ~instruction:"New text" ())
      ~card_id:RT.card_id
  in
  let paused = RT.serialised (card T.Paused)
  and suspended = RT.serialised (card T.Suspended)
  and active = RT.serialised (card T.Active) in
  check_bool "paused line" true (contains paused (paused_line "SO-3"));
  check_bool "paused has no suspended line" false
    (contains paused "is suspended, so");
  check_bool "suspended line" true (contains suspended (suspended_line "SO-3"));
  check_bool "suspended has no paused line" false
    (contains suspended "is paused, so");
  check_bool "active: no restart line" false
    (contains active "is paused, so" || contains active "is suspended, so")

let test_change_card_copy () =
  let instruction = String.make 1000 '&' in
  let label = String.concat "" (List.init 80 (fun _ -> "<")) in
  let r =
    Render.change_card ~replaces:(RT.order ()) ~grantor:"U456"
      ~action:(S.relay_action ~instruction ~target:{ S.claude with T.label } ())
      ~card_id:RT.card_id
  in
  check_string "fallback" "Change standing order SO-3 — confirm?" r.text;
  let all = String.concat "\n" (RT.mrkdwn_texts r) in
  List.iter
    (fun needle -> check_bool needle true (contains all needle))
    [
      "*Change standing order SO-3 — confirm?*";
      "Confirming cancels SO-3 and starts a new standing order with the \
       instruction below. The new order gets the next SO number.";
      "*When:* after every huddle in <#C123>";
      "*Runs as:* <@U456>.";
      "*Limits:* up to 10 relays a day";
      "Expires in 10 minutes. Only <@U456> can confirm.";
    ];
  Alcotest.(check (list (pair string (option string))))
    "buttons bound to the card"
    [
      (Render.action_change, Some RT.card_id);
      (Render.action_cancel, Some RT.card_id);
    ]
    (RT.button_ids r);
  check_string "change id" "clawq_so_change" Render.action_change;
  List.iter
    (fun obj ->
      check_bool "text object within the cap" true
        (RT.code_points (RT.text_of obj) <= Render.max_text_chars))
    (RT.text_objects r.blocks);
  check_int "the instruction in full, once" 1
    (List.length
       (List.filter
          (fun obj -> RT.text_of obj = instruction)
          (RT.text_objects r.blocks)));
  check_bool "no target mention" false (contains (RT.serialised r) "UCLAUDE");
  check_bool "a change card button is a card action" true
    (Slack_standing_order_cards.is_card_action Render.action_change)

(* ---- The tool ---- *)

let test_change_posts_card () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* out = change env () in
      check_string "reply"
        "A change card for SO-1 was posted in this thread. SO-1 is unchanged \
         until <@U123> presses Confirm change (expires in 10 minutes)."
        out;
      unchanged env o;
      check_int "no second row" 1 (List.length (all_orders env));
      (match C.posts_with env Render.action_change with
      | [ (params, ts) ] ->
          check_string "in the thread" F.thread_root
            (S.param params "thread_ts");
          check_bool "unfurls off" true
            (Slack_huddles.get "unfurl_links" params = `Bool false
            && Slack_huddles.get "unfurl_media" params = `Bool false);
          let card = change_card env o in
          check_opt "bound to the post" (Some ts) card.message_ts;
          check_opt "reply thread" (Some F.thread_root) card.reply_thread_ts;
          check_int "bound to the order's epoch" o.epoch card.order_epoch;
          check_string "requester" "U123" card.created_by;
          check_bool "expires in 10 minutes" true
            (card.expires_at = Some (S.now +. 600.));
          let plan =
            Option.bind card.plan_id (fun plan_id ->
                Setup_plan_apply.get_plan ~db:env.db ~plan_id)
          in
          (match plan with
          | Some p -> (
              check_opt "plan digest" (Some p.digest) card.plan_digest;
              check_string "plan principal" "U123" p.principal.id;
              match Change.draft_of_plan p with
              | Some d ->
                  check_string "replaces" o.id d.replaces;
                  check_bool "new text" true
                    (d.action = S.relay_action ~instruction:"New text" ())
              | None -> Alcotest.fail "the plan carries no change draft")
          | None -> Alcotest.fail "the card names no stored plan");
          check_int "planned in the ledger" 1
            (List.length (C.ledger env "standing_order_change_planned"))
      | posts ->
          Alcotest.failf "expected one change card, got %d" (List.length posts));
      Lwt.return_unit)

let test_only_grantor_or_admin_requests () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* out = change env ~user:"U456" () in
      check_string "refused" "Only <@U123> or a clawq admin can change SO-1."
        out;
      check_bool "no Slack call" true (F.methods env = []);
      unchanged env o;
      Lwt.return_unit)

let test_state_refusals () =
  F.with_orders (fun env ->
      let pending = F.seed env ~state:T.Pending_confirmation () in
      let* out = change env () in
      check_string "pending"
        "SO-1 is still waiting for confirmation; cancel it and create a new \
         one."
        out;
      ignore
        (Store.deactivate env.db ~order_id:pending.id (Store.Cancel "U123")
           ~now:S.now);
      let* out = change env () in
      check_string "cancelled"
        "SO-1 was cancelled; create a new standing order instead." out;
      ignore (F.seed env () : T.order);
      let* out = change env ~order:"SO-2" ~instruction:"" () in
      check_bool
        ("instruction validated: " ^ out)
        true
        (String.starts_with ~prefix:"Error: the instruction is empty" out);
      let* out = change env ~order:"SO-2" ~when_no:"sometimes" () in
      check_bool
        ("bad when_no_transcript: " ^ out)
        true
        (String.starts_with ~prefix:"Error:" out);
      check_bool "no Slack call" true (F.methods env = []);
      Lwt.return_unit)

let test_cross_channel () =
  F.with_orders (fun env ->
      ignore (F.seed env () : T.order);
      let far1 = S.seed env.db ~channel:"C999" () in
      let far2 =
        S.seed env.db ~channel:"C999"
          ~target:Test_slack_standing_order_tools.other_target ()
      in
      let* out2 = change env ~order:"SO-2" () in
      let* out9 = change env ~order:"SO-9" () in
      check_string "not found" (Render.not_found 2) out2;
      check_string "identical to a missing one"
        (Test_slack_standing_order_tools.replace_all ~sub:"SO-9" ~by:"SO-2" out9)
        out2;
      unchanged env far1;
      unchanged env far2;
      check_bool "no Slack call" true (F.methods env = []);
      Lwt.return_unit)

(* ---- The click ---- *)

let test_expired_card () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      S.at (S.now +. 601.);
      let* () = confirm_change env card in
      check_bool "expired reply" true
        (C.has_text env "This card has expired; ask clawq again for a new one.");
      unchanged env o;
      no_successor env;
      no_activation env;
      Lwt.return_unit)

let test_action_ts () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      let* () =
        Lwt_list.iter_s
          (fun action_ts -> confirm_change env ~action_ts card)
          [ None; Some ""; Some "abc" ]
      in
      check_int "each click refused" 3
        (List.length
           (List.filter
              (( = )
                 "Slack did not include a click time; press Confirm change \
                  again.")
              (F.texts env)));
      unchanged env o;
      no_successor env;
      Lwt.return_unit)

let test_swap () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let fire = S.fire env.db o ~room:"R1" () in
      let* card = planned env o in
      let* () = confirm_change env card in
      let old = C.find env o in
      Alcotest.check S.state_t "old cancelled" T.Cancelled old.state;
      check_opt "cancelled by the clicker" (Some "U123") old.cancelled_by;
      (match successor env with
      | Some n ->
          Alcotest.check S.state_t "new active" T.Active n.state;
          check_bool "new instruction, same target" true
            (n.action = S.relay_action ~instruction:"New text" ());
          check_string "grantor" "U123" n.grantor_user_id;
          check_opt "active_since is the click" (Some "1700000000.000200")
            n.active_since;
          check_opt "bot user" (Some "UBOT") n.bot_user_id;
          check_opt "bot" (Some "BOWN") n.bot_id;
          check_opt "confirmed by the clicker" (Some "U123") n.confirmed_by;
          check_int "epoch" 1 n.epoch;
          check_int "fresh cap" Store.default_max_fires_per_day
            n.max_fires_per_day;
          check_string "management thread" F.thread_root n.management_thread_ts;
          check_bool "pending window cleared" true (n.pending_expires_at = None);
          (match C.updates env with
          | [ p ] ->
              check_string "the card message" (C.posted_ts card)
                (S.param p "ts");
              check_string "closed"
                "Standing order SO-1 was replaced by SO-2 (by <@U123>)."
                (S.param p "text")
          | ps ->
              Alcotest.failf "expected one chat.update, got %d" (List.length ps));
          let button = F.card_for env ~order_id:n.id ~kind:T.Pause_button in
          check_bool "the new order's pause button is bound" true
            (button.message_ts <> None)
      | None -> Alcotest.fail "no SO-2");
      check_int "one activation notice" 1 (C.count_texts env "is active:");
      check_bool "the notice names SO-2" true
        (C.mentions env "Standing order SO-2 is active:");
      Alcotest.(check (list string))
        "only SO-2 is live" [ "SO-2" ]
        (List.map T.display_name (Store.list env.db ~channel_id:"C123" ()));
      let state, _, _ = S.fire_flags env.db fire.id in
      Alcotest.check S.fire_state_t "the old order's fire stops"
        T.Skipped_inactive state;
      check_bool "card used" true ((change_card env o).used_at <> None);
      (match C.ledger env "standing_order_changed" with
      | [ _ ] -> ()
      | rows ->
          Alcotest.failf "expected one changed row, got %d" (List.length rows));
      check_bool "the grantor changed their own order: no notice" false
        (C.mentions env "was replaced by SO-2 by");
      Lwt.return_unit)

let test_admin_becomes_grantor () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.admin env "U456";
      let* card = planned ~user:"U456" env o in
      let* () = confirm_change env ~user:"U456" card in
      (match successor env with
      | Some n ->
          check_string "the clicker is the grantor" "U456" n.grantor_user_id;
          Alcotest.check S.state_t "active" T.Active n.state
      | None -> Alcotest.fail "no SO-2");
      check_bool "the old grantor is told in the management thread" true
        (List.exists
           (fun (p, _) ->
             S.param p "text"
             = "<@U123> Standing order SO-1 was replaced by SO-2 by <@U456>."
             && S.param p "thread_ts" = F.thread_root)
           !(env.posted));
      Lwt.return_unit)

let test_bound_to_requester () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.admin env "U456";
      let* card = planned ~user:"U456" env o in
      let* () = confirm_change env ~user:"U123" card in
      check_bool "refused" true
        (C.has_text env "Only <@U456> can confirm this card.");
      unchanged env o;
      no_successor env;
      check_bool "card unused" true ((change_card env o).used_at = None);
      Lwt.return_unit)

let test_requester_must_still_be_admin () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      F.admin env "U456";
      let* card = planned ~user:"U456" env o in
      Sql_util.exec_with_params env.db
        "DELETE FROM channel_admins WHERE channel = 'slack' AND sender_id = ?"
        [ Sqlite3.Data.TEXT "U456" ];
      let before = C.mark env in
      let* () = confirm_change env ~user:"U456" card in
      check_bool "refused" true
        (C.has_text env "Only <@U123> or a clawq admin can change SO-1.");
      Alcotest.(check (list string))
        "only the refusal" [ "chat.postMessage" ] (C.calls_since env before);
      unchanged env o;
      no_successor env;
      Lwt.return_unit)

let test_stale_card () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      (match
         Store.deactivate env.db ~order_id:o.id (Store.Pause "U123") ~now:S.now
       with
      | Ok _ -> ()
      | Error e -> Alcotest.fail e);
      let paused = C.find env o in
      let* () = confirm_change env card in
      check_bool "out of date" true
        (C.has_text env
           "This change card is out of date; ask clawq for a new one.");
      unchanged env paused;
      Alcotest.check S.state_t "still paused" T.Paused paused.state;
      no_successor env;
      Lwt.return_unit)

let test_double_click () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      let* () = confirm_change env card in
      let posts = List.length !(env.posted) and before = C.mark env in
      let* () = confirm_change env card in
      let* () = cancel_change env card in
      check_int "two orders" 2 (List.length (all_orders env));
      check_int "one activation notice" 1 (C.count_texts env "is active:");
      check_int "one chat.update" 1 (List.length (C.updates env));
      check_int "no new post" posts (List.length !(env.posted));
      Alcotest.(check (list string))
        "no Slack call" [] (C.calls_since env before);
      Lwt.return_unit)

let shared_channel params =
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
    ]

let test_preflight () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      F.override env "conversations.info" shared_channel;
      let* () = confirm_change env card in
      check_bool "shared" true
        (C.has_text env
           "Not changed: this channel is shared with another organisation.");
      unchanged env o;
      no_successor env;
      no_activation env;
      Lwt.return_unit)

let deny_text = "this room's profile does not allow slack_huddle_content"

let test_capability_deny () =
  F.with_orders ~config:C.deny_content (fun env ->
      let o = F.seed env () in
      let* out = change env () in
      check_string "plan refused" ("Error: " ^ deny_text) out;
      check_bool "no Slack call" true (F.methods env = []);
      unchanged env o;
      Lwt.return_unit);
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      F.set_config env C.deny_content;
      let before = C.mark env in
      let* () = confirm_change env card in
      check_bool "click refused" true
        (C.has_text env ("Not changed: " ^ deny_text ^ "."));
      Alcotest.(check (list string))
        "only the refusal" [ "chat.postMessage" ] (C.calls_since env before);
      unchanged env o;
      no_successor env;
      Lwt.return_unit)

let test_cancel_change () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* card = planned env o in
      let* () = cancel_change env ~user:"U456" card in
      check_bool "not the requester" true
        (C.has_text env "Only <@U123> or a clawq admin can cancel this change.");
      check_bool "card unused" true ((change_card env o).used_at = None);
      let* () = cancel_change env card in
      unchanged env o;
      no_successor env;
      (match C.updates env with
      | [ p ] ->
          check_string "the card message" (C.posted_ts card) (S.param p "ts");
          check_string "closed"
            "The change to SO-1 was cancelled by <@U123>; SO-1 is unchanged."
            (S.param p "text")
      | ps -> Alcotest.failf "expected one chat.update, got %d" (List.length ps));
      check_bool "card used" true ((change_card env o).used_at <> None);
      let before = C.mark env in
      let* () = confirm_change env card in
      no_successor env;
      Alcotest.(check (list string))
        "a cancelled card is ignored" [] (C.calls_since env before);
      Lwt.return_unit)

let test_when_no_transcript_kept () =
  F.with_orders (fun env ->
      let o =
        let id = Store.new_id ~prefix:"so" in
        (match
           Store.insert_pending env.db ~id ~team_id:"T123" ~api_app_id:"A123"
             ~channel_id:"C123" ~grantor_user_id:"U123"
             ~action:(S.relay_action ~when_no:T.Skip ())
             ~management_thread_ts:F.thread_root ~now:S.now ()
         with
        | Ok _ -> ()
        | Error _ -> Alcotest.fail "seed a skip order");
        match
          Store.activate_in_tx env.db ~order_id:id
            ~active_since:"1700000000.000100" ~confirmed_by:"U123"
            ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:S.now
        with
        | Ok o -> o
        | Error e -> Alcotest.fail e
      in
      let* card = planned env o in
      check_bool "the card keeps skip" true
        (List.exists
           (fun (p, _) ->
             contains (Yojson.Safe.to_string p)
               "*No transcript?* Skip that huddle")
           (C.posts_with env Render.action_change));
      let* () = confirm_change env card in
      match successor env with
      | Some n ->
          check_bool "still skip" true
            (n.action
            = S.relay_action ~instruction:"New text" ~when_no:T.Skip ());
          Lwt.return_unit
      | None -> Alcotest.fail "no SO-2")

(* ---- Store.replace_in_tx ---- *)

let replace ?(target = S.claude) db (o : T.order) ~expected_epoch =
  Store.replace_in_tx db ~old_id:o.id ~expected_epoch
    ~new_id:(Store.new_id ~prefix:"so")
    ~grantor_user_id:"U123"
    ~action:(S.relay_action ~instruction:"New text" ~target ())
    ~management_thread_ts:"100.000001" ~active_since:"1700000000.000200"
    ~confirmed_by:"U123" ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:S.now ()

let count db =
  match
    Sql_util.query_rows db "SELECT COUNT(*) FROM slack_standing_orders"
      ~bind:(fun _ -> ())
      ~of_stmt:(fun stmt -> Sql_util.int_column stmt 0)
  with
  | [ n ] -> n
  | _ -> Alcotest.fail "count"

(* Inside the caller's BEGIN IMMEDIATE, then its ROLLBACK. *)
let test_replace_rolls_back () =
  let db = S.fresh_db () in
  let o = S.seed db () in
  let before = Store.find db o.id in
  let rolled_back f =
    Sql_util.exec_exn db "BEGIN IMMEDIATE";
    let result = f () in
    Sql_util.exec_exn db "ROLLBACK";
    check_bool "the old row is unchanged" true (Store.find db o.id = before);
    check_int "no new row" 1 (count db);
    result
  in
  let stale =
    rolled_back (fun () -> replace db o ~expected_epoch:(o.epoch - 1))
  in
  check_bool "a stale epoch is an error" true (Result.is_error stale);
  let other =
    rolled_back (fun () ->
        replace ~target:Test_slack_standing_order_tools.other_target db o
          ~expected_epoch:o.epoch)
  in
  check_bool "another target is an error" true (Result.is_error other);
  match rolled_back (fun () -> replace db o ~expected_epoch:o.epoch) with
  | Ok ((old : T.order), (fresh : T.order)) ->
      Alcotest.check S.state_t "cancelled inside" T.Cancelled old.state;
      Alcotest.check S.state_t "active inside" T.Active fresh.state;
      check_int "the next number" 2 fresh.display_no;
      check_int "a confirmed order's epoch" 1 fresh.epoch
  | Error e -> Alcotest.fail ("replace failed: " ^ e)

let suite =
  [
    S.case "change card discloses a restart" test_restart_disclosed;
    S.case "change card copy" test_change_card_copy;
    S.case "expired change card" test_expired_card;
    S.case "change needs a valid action_ts" test_action_ts;
    S.case "change posts a card and changes nothing" test_change_posts_card;
    S.case "only the grantor or an admin can request a change"
      test_only_grantor_or_admin_requests;
    S.case "change refuses pending, cancelled and bad arguments"
      test_state_refusals;
    S.case "confirm change swaps atomically" test_swap;
    S.case "admin requester becomes grantor" test_admin_becomes_grantor;
    S.case "change card is bound to its requester" test_bound_to_requester;
    S.case "a requester who lost admin can't confirm"
      test_requester_must_still_be_admin;
    S.case "stale change card" test_stale_card;
    S.case "double click changes once" test_double_click;
    S.case "change reruns the preflight" test_preflight;
    S.case "capability deny at plan and at click" test_capability_deny;
    S.case "cross-channel change looks like not found" test_cross_channel;
    S.case "cancel on a change card changes nothing" test_cancel_change;
    S.case "replace_in_tx rolls back" test_replace_rolls_back;
    S.case "when_no_transcript is kept unless given"
      test_when_no_transcript_kept;
  ]
