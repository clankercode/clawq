(* Standing-order management tools (spec §5.4, §6.8, §7.6-7.8): the shared
   prelude, create's plan and bound confirm card, the channel-bound list, show,
   pause, resume and cancel, and queued-drain refusal through real ingress. *)
open Lwt.Syntax
module Tools = Slack_standing_order_tools
module F = Test_slack_standing_order_fixtures
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Render = Slack_standing_order_render
module G = Slack_standing_order_grant

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)
let contains = Test_helpers.string_contains
let get = Slack_huddles.get

let expect_error what out =
  check_bool
    (what ^ " is an error: " ^ out)
    true
    (String.starts_with ~prefix:"Error:" out)

let all_orders (env : F.env) = Store.list env.db ~include_cancelled:true ()

let orders_in (env : F.env) channel =
  Store.list env.db ~channel_id:channel ~include_cancelled:true ()

let find (env : F.env) (o : T.order) =
  match Store.find env.db o.id with
  | Some o -> o
  | None -> Alcotest.fail ("no order " ^ o.id)

let check_state env o want =
  Alcotest.check S.state_t "order state" want (find env o).state

(* Accepted posts whose params mention [needle] (an action id, say). *)
let posts_with (env : F.env) needle =
  List.filter
    (fun (params, _) -> contains (Yojson.Safe.to_string params) needle)
    !(env.posted)

let order_arg n = `Assoc [ ("order", `String n) ]
let no_args = `Assoc []

let replace_all ~sub ~by s =
  let n = String.length sub and len = String.length s in
  let b = Buffer.create len in
  let rec go i =
    if i > len - n then Buffer.add_string b (String.sub s i (len - i))
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let other_target : T.target =
  {
    user_id = "UOTHER";
    app_id = "AOTHER";
    bot_id = "BOTHER";
    label = "Other bot";
  }

let add_bot (env : F.env) ~id ~app ~bot ?(name = id) () =
  env.world.users :=
    !(env.world.users)
    @ [
        ( id,
          S.user ~id ~is_bot:true
            [
              ("name", S.str name);
              ( "profile",
                S.obj [ ("api_app_id", S.str app); ("bot_id", S.str bot) ] );
            ] );
      ]

let labels = "Claude (Anthropic)"

let no_match_text =
  "Error: no approved standing-order target matches that name. Approved \
   targets here: " ^ labels ^ ". Name one of them, or mention the bot."

let not_allowlisted_text =
  "Error: that bot is not an approved standing-order target here. Approved \
   targets here: " ^ labels
  ^ ". An operator adds targets in \
     channels.slack.standing_orders.allowed_targets."

let bad_argument_text =
  "Error: \"target\" must be an approved bot's name or a mention of it; \
   channel mentions and other Slack links are not targets. Approved targets \
   here: " ^ labels ^ "."

let listing_incomplete_text =
  "Error: this workspace has too many users for clawq to search by name; \
   mention the bot as the target instead."

(* ---- create ---- *)

let test_create_card () =
  F.with_orders (fun env ->
      let* out = F.create env () in
      check_string "reply"
        "A confirm card for SO-1 was posted in this thread. Nothing is active \
         until <@U123> presses Confirm (expires in 10 minutes)."
        out;
      match orders_in env "C123" with
      | [ o ] -> (
          Alcotest.check S.state_t "pending" T.Pending_confirmation o.state;
          check_string "grantor" "U123" o.grantor_user_id;
          check_bool "target" true (T.action_target o.action = S.claude);
          check_string "management thread" F.thread_root o.management_thread_ts;
          match posts_with env Render.action_confirm with
          | [ (params, ts) ] ->
              check_string "in the thread" F.thread_root
                (S.param params "thread_ts");
              check_bool "unfurls off" true
                (get "unfurl_links" params = `Bool false
                && get "unfurl_media" params = `Bool false);
              let card = F.card_for env ~order_id:o.id ~kind:T.Confirm_card in
              Alcotest.(check (option string))
                "card bound to the post" (Some ts) card.message_ts;
              Alcotest.(check (option string))
                "reply thread" (Some F.thread_root) card.reply_thread_ts;
              let plan =
                Option.bind card.plan_id (fun plan_id ->
                    Setup_plan_apply.get_plan ~db:env.db ~plan_id)
              in
              (match plan with
              | Some p ->
                  Alcotest.(check (option string))
                    "plan digest" (Some p.digest) card.plan_digest
              | None -> Alcotest.fail "the card names no stored plan");
              check_bool "planned in the ledger" true
                (Room_activity_ledger.query ~db:env.db ~room_id:"slack:C123"
                   ~event_type:"standing_order_planned" ()
                <> []);
              Lwt.return_unit
          | posts ->
              Alcotest.failf "expected one confirm card, got %d"
                (List.length posts))
      | os -> Alcotest.failf "expected one C123 order, got %d" (List.length os))

let test_live_turn () =
  F.with_orders (fun env ->
      let attempt mode =
        F.invoke env ~mode "slack_standing_order_create" (F.create_args ())
      in
      let* bare = attempt `Bare in
      let* steered = attempt `Steered in
      let* drained = attempt `Drained in
      List.iter
        (fun (what, out) ->
          check_bool
            (what ^ ": " ^ out)
            true
            (String.starts_with ~prefix:"Error: Slack huddle access requires"
               out))
        [ ("bare", bare); ("steered", steered) ];
      check_string "drained"
        "Error: Standing orders can't be managed from a queued request; send \
         it again in Slack."
        drained;
      check_bool "nothing stored" true (all_orders env = []);
      check_bool "no Slack call" true (F.methods env = []);
      Lwt.return_unit)

let test_identity_args () =
  F.with_orders (fun env ->
      let* () =
        Lwt_list.iter_s
          (fun (key, value) ->
            let* out = F.create env ~args:[ (key, `String value) ] () in
            expect_error key out;
            Lwt.return_unit)
          [ ("channel_id", "C999"); ("grantor", "U456"); ("team_id", "T999") ]
      in
      let* out = F.invoke env "slack_standing_order_list" (order_arg "SO-1") in
      expect_error "list with an argument" out;
      let* out = F.invoke env "slack_standing_order_list" `Null in
      check_bool
        ("null is no arguments: " ^ out)
        true
        (Slack_huddles.get "orders" (Yojson.Safe.from_string out) = `List []);
      check_bool "no Slack call" true (F.methods env = []);
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

let test_instruction () =
  F.with_orders (fun env ->
      let instruction s = [ ("instruction", `String s) ] in
      let* long = F.create env ~args:(instruction (String.make 1001 'a')) () in
      expect_error "1001 characters" long;
      check_bool ("states the limit: " ^ long) true (contains long "1000");
      let* () =
        Lwt_list.iter_s
          (fun (what, s) ->
            let* out = F.create env ~args:(instruction s) () in
            expect_error what out;
            Lwt.return_unit)
          [ ("empty", ""); ("invalid UTF-8", "\xff"); ("control", "a\x07") ]
      in
      let* out = F.create env ~args:[ ("instruction", `Int 3) ] () in
      expect_error "not a string" out;
      check_bool "nothing stored" true (all_orders env = []);
      check_bool "no card" true (posts_with env Render.action_confirm = []);
      let emoji = String.concat "" (List.init 1000 (fun _ -> "😀")) in
      let* out = F.create env ~args:(instruction emoji) () in
      check_bool
        ("1000 code points: " ^ out)
        true
        (contains out "Nothing is active");
      Alcotest.(check int)
        "one card" 1
        (List.length (posts_with env Render.action_confirm));
      Lwt.return_unit)

let test_unapproved_and_duplicate () =
  F.with_orders (fun env ->
      let target t = [ ("target", `String t) ] in
      let* out = F.create env ~args:(target "Gemini") () in
      check_string "no match" no_match_text out;
      add_bot env ~id:"UOTHER" ~app:"AOTHER" ~bot:"BOTHER" ();
      let* out = F.create env ~args:(target "<@UOTHER>") () in
      check_string "not allowlisted" not_allowlisted_text out;
      check_bool "the seeded bot was looked up" true
        (List.exists
           (fun (m, p) -> m = "users.info" && S.param p "user" = "UOTHER")
           !(env.calls));
      let* out = F.create env ~args:(target "<!channel>") () in
      check_string "bad argument" bad_argument_text out;
      check_bool "no card" true (posts_with env Render.action_confirm = []);
      check_bool "nothing stored" true (all_orders env = []);
      ignore (F.seed env () : T.order);
      let* out = F.create env () in
      check_bool ("duplicate: " ^ out) true
        (String.starts_with ~prefix:"Error: " out
        && contains out "SO-1 already relays huddles");
      F.override env "users.list" (fun _ ->
          S.ok
            [
              ("members", `List []);
              ("response_metadata", S.obj [ ("next_cursor", S.str "more") ]);
            ]);
      let* out = F.create env ~args:(target "Somebot") () in
      check_string "endless listing" listing_incomplete_text out;
      check_bool "only the seeded order" true (List.length (all_orders env) = 1);
      Lwt.return_unit)

let test_target_lookup_texts () =
  F.with_orders (fun env ->
      add_bot env ~id:"UCLAUDE2" ~app:"ACLAUDE" ~bot:"BCLAUDE2" ~name:"claude"
        ();
      let* out = F.create env () in
      check_string "ambiguous"
        "Error: that name matches several approved bots: Claude (Anthropic) \
         (UCLAUDE), Claude (Anthropic) (UCLAUDE2). Run again with target set \
         to one of those user ids."
        out;
      check_bool "no mention markup" false (contains out "<@");
      F.override env "users.list" (fun _ ->
          S.obj [ ("ok", `Bool false); ("error", S.str "ratelimited") ]);
      let* out = F.create env () in
      check_string "lookup failed"
        "Error: clawq could not look up Slack bots (rate_limited); try again \
         in a minute."
        out;
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

let test_profile_denies_tool () =
  F.with_orders
    ~config:(fun c -> S.profile c ~denied:[ "slack_standing_order_create" ] ())
    (fun env ->
      let* out = F.create env () in
      expect_error "denied tool" out;
      check_bool "no Slack call" true (F.methods env = []);
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

let test_capability_deny () =
  F.with_orders
    ~config:(fun c -> S.profile c ~denied:[ "slack_huddle_content" ] ())
    (fun env ->
      let* out = F.create env () in
      check_string "capability deny"
        "Error: this room's profile does not allow slack_huddle_content" out;
      check_bool "no Slack call" true (F.methods env = []);
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

let test_targets_reload () =
  F.with_orders (fun env ->
      let* out = F.create env ~args:[ ("target", `String "Gemini") ] () in
      check_string "before the reload" no_match_text out;
      F.set_config env (fun c ->
          S.with_slack c (fun sc ->
              {
                sc with
                standing_orders =
                  { sc.standing_orders with allowed_targets = [] };
              }));
      let* out = F.create env () in
      check_string "after the reload"
        ("Error: " ^ Render.reason_text G.Target_not_allowlisted)
        out;
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

(* ---- list, show and the channel binding ---- *)

(* C123 SO-1; C999 SO-1 (paused) and SO-2 (active, another target). *)
let seed_two_channels (env : F.env) =
  let own = F.seed env () in
  let far1 = S.seed env.db ~channel:"C999" ~state:T.Paused () in
  let far2 = S.seed env.db ~channel:"C999" ~target:other_target () in
  Alcotest.(check int) "C999 SO-2" 2 far2.display_no;
  (own, far1, far2)

let test_list_show_bound () =
  F.with_orders (fun env ->
      let own, _, _ = seed_two_channels env in
      let* listed = F.invoke env "slack_standing_order_list" no_args in
      let json = Yojson.Safe.from_string listed in
      let names =
        match get "orders" json with
        | `List os -> List.map (fun o -> Slack_huddles.text (get "order" o)) os
        | _ -> Alcotest.fail ("list: no orders array: " ^ listed)
      in
      Alcotest.(check (list string)) "only this channel's" [ "SO-1" ] names;
      check_bool "note" true (get "note" json = `String Render.untrusted_note);
      ignore
        (Store.deactivate env.db ~order_id:own.id (Store.Cancel "U123")
           ~now:S.now);
      let* listed = F.invoke env "slack_standing_order_list" no_args in
      check_bool
        ("cancelled orders are left out: " ^ listed)
        true
        (get "orders" (Yojson.Safe.from_string listed) = `List []);
      let* show2 =
        F.invoke env "slack_standing_order_show" (order_arg "SO-2")
      in
      let* show9 =
        F.invoke env "slack_standing_order_show" (order_arg "SO-9")
      in
      check_string "another channel's order is not found" (Render.not_found 2)
        show2;
      check_string "identical to a missing one"
        (replace_all ~sub:"SO-9" ~by:"SO-2" show9)
        show2;
      let* bad = F.invoke env "slack_standing_order_show" (order_arg "nine") in
      expect_error "a malformed name" bad;
      Lwt.return_unit)

let test_mutations_bound () =
  F.with_orders (fun env ->
      let _, far1, far2 = seed_two_channels env in
      let* () =
        Lwt_list.iter_s
          (fun tool ->
            let name = "slack_standing_order_" ^ tool in
            let* out2 = F.invoke env name (order_arg "SO-2") in
            let* out9 = F.invoke env name (order_arg "SO-9") in
            check_string (tool ^ " is identical")
              (replace_all ~sub:"SO-9" ~by:"SO-2" out9)
              out2;
            check_string (tool ^ " is not found") (Render.not_found 2) out2;
            Lwt.return_unit)
          [ "pause"; "resume"; "cancel" ]
      in
      List.iter
        (fun (o : T.order) ->
          let now = find env o in
          Alcotest.check S.state_t "C999 state kept" o.state now.state;
          Alcotest.(check int) "C999 epoch kept" o.epoch now.epoch)
        [ far1; far2 ];
      check_bool "no post, user or channel read" true
        (List.for_all
           (fun m ->
             not
               (m = "chat.postMessage" || m = "users.info"
               || String.starts_with ~prefix:"conversations." m))
           (F.methods env));
      Lwt.return_unit)

let test_show_untrusted () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* out = F.invoke env "slack_standing_order_show" (order_arg "SO-1") in
      let json = Yojson.Safe.from_string out in
      check_bool "untrusted instruction" true
        (get "untrusted_text" (get "instruction" json)
        = `String "Update the project docs.");
      check_bool "no opaque id" false (contains out o.id);
      check_bool "recent fires" true (get "recent_fires" json = `List []);
      Lwt.return_unit)

(* ---- pause, resume, cancel ---- *)

let test_pause_tool () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* out = F.invoke env "slack_standing_order_pause" (order_arg "SO-1") in
      check_state env o T.Paused;
      check_bool ("receipt: " ^ out) true (contains out "is paused");
      (match List.filter (fun t -> t = out) (F.texts env) with
      | [ _ ] -> ()
      | _ -> Alcotest.fail "the receipt was not posted once");
      check_bool "receipt in the thread" true
        (List.exists
           (fun (p, _) ->
             S.param p "text" = out && S.param p "thread_ts" = F.thread_root)
           !(env.posted));
      Lwt.return_unit)

let test_resume_card_only () =
  F.with_orders (fun env ->
      let o = F.seed env ~state:T.Paused () in
      let* out =
        F.invoke env "slack_standing_order_resume" (order_arg "SO-1")
      in
      check_string "reply"
        "A resume card for SO-1 was posted; only <@U123> or a clawq admin can \
         resume it."
        out;
      check_state env o T.Paused;
      match posts_with env Render.action_resume with
      | [ (params, ts) ] ->
          check_string "in the thread" F.thread_root
            (S.param params "thread_ts");
          let card = F.card_for env ~order_id:o.id ~kind:T.Resume_card in
          Alcotest.(check int) "card epoch" (find env o).epoch card.order_epoch;
          Alcotest.(check (option string)) "bound" (Some ts) card.message_ts;
          let active = S.seed env.db ~target:other_target () in
          let* out =
            F.invoke env "slack_standing_order_resume" (order_arg "SO-2")
          in
          check_string "nothing to resume"
            "Standing order SO-2 is already active; there is nothing to resume."
            out;
          check_state env active T.Active;
          Alcotest.(check int)
            "still one resume card" 1
            (List.length (posts_with env Render.action_resume));
          Lwt.return_unit
      | posts ->
          Alcotest.failf "expected one resume card, got %d" (List.length posts))

let test_cancel_authority () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let* out =
        F.invoke env ~user:"U456" "slack_standing_order_cancel"
          (order_arg "SO-1")
      in
      check_string "not the grantor"
        "Only <@U123> or a clawq admin can cancel SO-1." out;
      check_state env o T.Active;
      let* out =
        F.invoke env "slack_standing_order_cancel" (order_arg "SO-1")
      in
      check_state env o T.Cancelled;
      check_bool ("receipt: " ^ out) true (contains out "is cancelled");
      check_bool "receipt posted" true (List.mem out (F.texts env));
      let second = F.seed env () in
      F.admin env "U456";
      let* out =
        F.invoke env ~user:"U456" "slack_standing_order_cancel"
          (order_arg "SO-2")
      in
      check_state env second T.Cancelled;
      check_bool ("admin receipt: " ^ out) true (contains out "is cancelled");
      check_bool "grantor told" true
        (List.mem
           (Render.grantor_notice (find env second) (Render.Cancelled_by "U456"))
           (F.texts env));
      check_bool "cancel ledger" true
        (List.length
           (Room_activity_ledger.query ~db:env.db ~room_id:"slack:C123"
              ~event_type:"standing_order_cancelled" ())
        = 2);
      Lwt.return_unit)

let test_cancel_member_check () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      S.remove_member env.world "U123";
      let* out =
        F.invoke env "slack_standing_order_cancel" (order_arg "SO-1")
      in
      check_string "not a member"
        ("Not cancelled: "
        ^ Render.member_refusal_text ~action:`Cancel G.Member_not_member
        ^ ".")
        out;
      check_state env o T.Active;
      Lwt.return_unit)

let test_registered () =
  F.with_orders (fun env ->
      let registry = Option.get env.base.manager.Session_core.tool_registry in
      Alcotest.(check int) "six tools" 6 (List.length Tools.tool_names);
      List.iter
        (fun name ->
          check_bool name true (Tool_registry.find registry name <> None))
        Tools.tool_names;
      (match Tool_registry.find registry "slack_standing_order_create" with
      | Some t -> check_bool "create is Medium" true (t.risk_level = Tool.Medium)
      | None -> Alcotest.fail "no create tool");
      List.iter
        (fun name ->
          match Tool_registry.find registry name with
          | Some t ->
              check_bool (name ^ " is closed") true
                (get "additionalProperties" t.parameters_schema = `Bool false)
          | None -> ())
        Tools.tool_names;
      Lwt.return_unit)

(* ---- the drained flag through real ingress (ruling F1) ---- *)

let text_response model content =
  Provider.Text
    {
      content;
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

let tool_call model name args =
  Provider.ToolCalls
    {
      calls =
        [
          {
            Provider.id = "call-" ^ name;
            function_name = name;
            arguments = Yojson.Safe.to_string args;
          };
        ];
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

(* "HOLD" waits for [hold]; "MGMT-CREATE" calls create once; a tool result
   ends the turn. *)
let script ?(hold = Lwt.return_unit) (env : F.env) =
  env.provider :=
    fun ~model ~messages ->
      env.seen := messages :: !(env.seen);
      match List.rev messages with
      | { Provider.role = "tool"; _ } :: _ ->
          Lwt.return (text_response model "done")
      | newest_first -> (
          match
            List.find_opt
              (fun (m : Provider.message) -> m.role = "user")
              newest_first
          with
          | Some m when contains m.content "HOLD" ->
              Lwt.map (fun () -> text_response model "held") hold
          | Some m when contains m.content "MGMT-CREATE" ->
              Lwt.return
                (tool_call model "slack_standing_order_create"
                   (F.create_args ()))
          | _ -> Lwt.return (text_response model "nothing to do"))

let tool_results (env : F.env) =
  List.concat_map
    (List.filter_map (fun (m : Provider.message) ->
         if m.role = "tool" then Some m.content else None))
    !(env.seen)

let test_live_ingress () =
  F.with_orders (fun env ->
      script env;
      let* _ = F.say env ~ts:F.thread_root "MGMT-CREATE" in
      check_bool "the tool ran" true (tool_results env <> []);
      check_bool "a live native turn manages orders" true
        (List.exists
           (fun r -> contains r "Nothing is active")
           (tool_results env));
      Alcotest.(check int) "one pending order" 1 (List.length (all_orders env));
      Lwt.return_unit)

let test_drained_ingress () =
  F.with_orders (fun env ->
      let hold, release = Lwt.wait () in
      script ~hold env;
      let first = F.say env ~ts:F.thread_root "HOLD" in
      let* () = Test_slack_socket.settle 3 in
      check_bool "the first turn holds" true (Lwt.is_sleeping first);
      let* _ = F.say env ~thread:F.thread_root ~ts:"100.000003" "MGMT-CREATE" in
      Lwt.wakeup_later release ();
      let* _ = first in
      check_bool "the queued create ran" true (tool_results env <> []);
      check_bool "a queued drain is refused" true
        (List.exists (fun r -> contains r "queued request") (tool_results env));
      check_bool "nothing stored" true (all_orders env = []);
      Lwt.return_unit)

(* ---- builtin agent templates (spec §6.8: no template creates, changes,
   resumes or cancels) ---- *)

let test_ops_template () =
  let read_and_pause = [ Tools.list_name; Tools.show_name; Tools.pause_name ]
  and grant_tools =
    [
      Tools.create_name;
      Slack_standing_order_change.tool_name;
      Tools.resume_name;
      Tools.cancel_name;
    ]
  in
  (match Agent_template_builtins.find "ops" with
  | None -> Alcotest.fail "no ops template"
  | Some t ->
      List.iter
        (fun name ->
          check_bool ("ops allows " ^ name) true (List.mem name t.allowed_tools))
        read_and_pause);
  List.iter
    (fun (t : Agent_template.t) ->
      List.iter
        (fun name ->
          check_bool
            (t.name ^ " never allows " ^ name)
            false
            (List.mem name t.allowed_tools))
        grant_tools)
    Agent_template_builtins.all

let suite =
  List.map
    (fun (name, f) -> (name, `Quick, f))
    [
      ("ops template exposes read and pause only", test_ops_template);
      ("create posts a bound card", test_create_card);
      ("create needs a live turn", test_live_turn);
      ("identity arguments are rejected", test_identity_args);
      ("create validates the instruction", test_instruction);
      ("unapproved and duplicate", test_unapproved_and_duplicate);
      ("target lookup failures are fixed texts", test_target_lookup_texts);
      ("profile denies the tool", test_profile_denies_tool);
      ("capability deny at plan", test_capability_deny);
      ("targets reload live", test_targets_reload);
      ("list and show are channel bound", test_list_show_bound);
      ("mutations are channel bound", test_mutations_bound);
      ("show marks the instruction untrusted", test_show_untrusted);
      ("pause tool", test_pause_tool);
      ("resume tool posts a card only", test_resume_card_only);
      ("cancel authority", test_cancel_authority);
      ("cancel needs channel membership", test_cancel_member_check);
      ("tools are registered", test_registered);
      ("a live native turn is not drained", test_live_ingress);
      ("a queued native drain is refused", test_drained_ingress);
    ]
