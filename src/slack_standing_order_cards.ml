(** Bound card clicks (spec §6.8): Confirm and Cancel on a confirm card, Resume
    on a resume card, Pause on an activation notice, the activation notice
    itself, and the binding for a change card's Confirm change and Cancel, which
    [Slack_standing_order_change] handles.

    A click acts only when it is bound to its persisted card: the card exists
    and its kind matches the button, a plan card names its plan, the payload's
    team, app, channel and message ts are the card's, and the clicker is a Slack
    user id. Any other click is logged and ignored with no Slack call, as for
    [clawq_stop_run]. Natural language never reaches this module, so only a
    bound click confirms or resumes. Replies are fixed templates in the card's
    thread, and a Slack write never undoes a state change. *)

open Lwt.Syntax
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Render = Slack_standing_order_render
module Pause = Slack_standing_order_pause
module Tools = Slack_standing_order_tools
module Change = Slack_standing_order_change

let field = Slack_huddles.field
let get = Slack_huddles.get

type button =
  | Press_confirm
  | Press_cancel
  | Press_resume
  | Press_pause
  | Press_change

let button_of_action_id id =
  List.assoc_opt id
    [
      (Render.action_confirm, Press_confirm);
      (Render.action_cancel, Press_cancel);
      (Render.action_resume, Press_resume);
      (Render.action_pause, Press_pause);
      (Render.action_change, Press_change);
    ]

let is_card_action id = Option.is_some (button_of_action_id id)

let card_kinds = function
  | Press_confirm -> [ T.Confirm_card ]
  | Press_cancel -> [ T.Confirm_card; T.Change_card ]
  | Press_resume -> [ T.Resume_card ]
  | Press_pause -> [ T.Pause_button ]
  | Press_change -> [ T.Change_card ]

let has_plan_binding = function
  | T.Confirm_card | T.Change_card -> true
  | T.Resume_card | T.Pause_button -> false

(* The audit unit of every Slack call a click on this card makes. *)
let unit_label = function
  | T.Confirm_card -> "confirm"
  | T.Resume_card -> "resume"
  | T.Pause_button -> "pause"
  | T.Change_card -> "change"

let ignored why =
  Logs.info (fun m -> m "standing-order card click ignored: %s" why);
  Lwt.return_unit

(* ── Slack writes (best effort: a failure is a warning, never an exception) ─ *)

let guarded ~what ~channel_id f =
  Lwt.catch f (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | exn ->
        Lwt.return
          (Error
             (Printf.sprintf "%s in %s: exception: %s" what channel_id
                (Printexc.to_string exn))))

(* The posted message's ts; a post without a valid one failed. *)
let post_result (call : SO.call) ~what ~channel_id ?thread_ts ?blocks text =
  guarded ~what ~channel_id (fun () ->
      let+ result =
        call "chat.postMessage"
          ([ ("channel", `String channel_id) ]
          @ (match thread_ts with
            | Some ts -> [ ("thread_ts", `String ts) ]
            | None -> [])
          @ [ ("text", `String text) ]
          @ (match blocks with Some b -> [ ("blocks", b) ] | None -> [])
          @ [ ("unfurl_links", `Bool false); ("unfurl_media", `Bool false) ])
      in
      match result with
      | Ok json -> (
          match get "ts" json with
          | `String ts when T.valid_slack_ts ts -> Ok ts
          | _ -> Error "no_ts")
      | Error e -> Error (SO.error_code e))

let warn_unposted ~what ~channel_id = function
  | Ok _ -> ()
  | Error code ->
      Logs.warn (fun m ->
          m "standing-order %s not posted in %s: %s" what channel_id code)

let post call ~what ~channel_id ?thread_ts text =
  Lwt.map
    (warn_unposted ~what ~channel_id)
    (post_result call ~what ~channel_id ?thread_ts text)

(* ── Activation notice ──────────────────────────────────────────────────── *)

let notice_failed_actor = "system:activation_notice_failed"

(* An order nobody was told about must not relay: pause it and tell the
   grantor in the management thread. *)
let pause_unannounced ~db ~(call : SO.call) ~now (o : T.order) code =
  Logs.warn (fun m ->
      m "standing-order %s activation notice not posted (%s); pausing it" o.id
        code);
  match
    try
      Store.deactivate db ~order_id:o.id (Store.Pause notice_failed_actor) ~now
    with exn -> Error (Printexc.to_string exn)
  with
  | Ok { Store.changed = true; after; _ } ->
      Pause.ledger ~db ~channel_id:o.channel_id ~actor:notice_failed_actor
        ~event_type:"standing_order_paused"
        [
          ("order", `String (T.display_name after));
          ("source", `String "activation_notice_failed");
        ];
      post call ~what:"notice_failed" ~channel_id:after.management_channel_id
        ~thread_ts:after.management_thread_ts
        (Render.grantor_notice after Render.Notice_failed)
  | Ok { Store.changed = false; _ } -> Lwt.return_unit
  | Error e ->
      Logs.err (fun m ->
          m
            "standing-order %s is active without a channel notice and could \
             not be paused: %s"
            o.id e);
      Lwt.return_unit

(* A top-level notice (no thread, unfurls off) with a bound Pause button. The
   button card keeps [reply_thread_ts] empty, so its receipts go to the
   notice's own thread. *)
let announce_activation ~db ~(call : SO.call) ~now (o : T.order) =
  Lwt.catch
    (fun () ->
      let card =
        Store.insert_card db ~kind:T.Pause_button ~order:o
          ~created_by:o.grantor_user_id ~now ()
      in
      let notice = Render.activation_notice o ~card_id:card.id in
      let* posted =
        post_result call ~what:"activation_notice" ~channel_id:o.channel_id
          ~blocks:notice.blocks notice.text
      in
      match posted with
      | Ok ts ->
          Store.set_card_message_ts db ~card_id:card.id ts;
          Lwt.return_unit
      | Error code -> pause_unannounced ~db ~call ~now o code)
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn ->
          pause_unannounced ~db ~call ~now o
            ("exception: " ^ Printexc.to_string exn))

(* ── Binding ────────────────────────────────────────────────────────────── *)

type click = {
  manager : Session.t;
  db : Sqlite3.db;
  now : float;
  card : T.card;
  order : T.order;  (** The card's order as it was when the click arrived. *)
  clicker : string;
  message_ts : string;  (** The card's own message. *)
  action_ts : string option;  (** Slack's click time, when it is one. *)
}

let bind ~manager ~db ~now ~payload ~button action =
  let ( let* ) = Result.bind in
  let require ok why = if ok then Ok () else Error why in
  let container = get "container" payload in
  let message_ts = field "message_ts" container in
  let clicker = field "id" (get "user" payload) in
  let* (card : T.card) =
    Option.to_result ~none:"unknown card"
      (Store.find_card db (field "value" action))
  in
  let* () =
    require
      (List.mem card.kind (card_kinds button))
      "the card kind does not match the button"
  in
  let* () =
    require
      ((not (has_plan_binding card.kind))
      || (Option.is_some card.plan_id && Option.is_some card.plan_digest))
      "the card names no plan"
  in
  let* () =
    require
      (card.team_id = field "id" (get "team" payload)
      && card.app_id = field "api_app_id" payload
      && card.channel_id = field "channel_id" container)
      "another team, app or channel"
  in
  let* () =
    require
      (card.message_ts = Some message_ts
      && field "ts" (get "message" payload) = message_ts)
      "another message"
  in
  let* () =
    require
      (Slack_config_types.valid_id "UW" clicker)
      "the clicker is not a Slack user id"
  in
  let* (order : T.order) =
    match Store.find db card.order_id with
    | Some o
      when o.team_id = card.team_id && o.api_app_id = card.app_id
           && o.channel_id = card.channel_id ->
        Ok o
    | _ -> Error "the card's order is unavailable"
  in
  let action_ts =
    match get "action_ts" action with
    | `String ts when T.valid_slack_ts ts -> Some ts
    | _ -> None
  in
  Ok { manager; db; now; card; order; clicker; message_ts; action_ts }

(* ── Click helpers ──────────────────────────────────────────────────────── *)

let current_config c () = Session.get_config c.manager
let reply_thread c = Option.value c.card.reply_thread_ts ~default:c.message_ts

let card_open c =
  match c.card.expires_at with Some t -> t > c.now | None -> false

let is_admin c = Admin.is_admin ~db:c.db ~channel:"slack" ~sender_id:c.clicker

let make_call c snapshot =
  SO.make
    ~priority:
      (if c.card.kind = T.Pause_button then Slack_read_limiter.Pause else Live)
    ~db:c.db ~current_config:(current_config c) ~snapshot
    ~channel_id:c.card.channel_id
    ~audit:
      (SO.audit_label ~order_id:(Some c.order.id)
         ~unit_label:(unit_label c.card.kind))
    ()

(* A call under a fresh persisted snapshot for this order. *)
let fresh_call c =
  Result.map (make_call c)
    (SO.snapshot ~db:c.db ~config:(current_config c ())
       ~channel_id:c.card.channel_id ~order_ref:(Pause.order_ref c.order))

let reply_with call c text =
  post call
    ~what:(unit_label c.card.kind ^ "_reply")
    ~channel_id:c.card.channel_id ~thread_ts:(reply_thread c) text

let reply c text =
  match fresh_call c with
  | Ok call -> reply_with call c text
  | Error code ->
      Logs.warn (fun m ->
          m "standing-order %s click reply not posted: %s" c.order.id code);
      Lwt.return_unit

(* Replaces the card's blocks, so its buttons disappear. *)
let close_card call c closed =
  Tools.update_card call ~order_id:c.order.id ~channel_id:c.card.channel_id
    ~ts:c.message_ts closed

let use_card c =
  ignore (Store.use_card c.db ~card_id:c.card.id ~now:c.now : bool)

let ledger c ~event_type (o : T.order) =
  Pause.ledger ~db:c.db ~channel_id:o.channel_id ~actor:c.clicker ~event_type
    [ ("order", `String (T.display_name o)); ("source", `String "button") ]

(* ── Confirm ────────────────────────────────────────────────────────────── *)

(* Steps 6-7: the compare-and-swap on plan id and digest, under the config
   revision the plan was made with. *)
let apply_confirm c (v : Grant.validated) (o : T.order) ~action_ts ~plan_id
    ~digest =
  let activated = ref None in
  let authority ~(principal : Setup_plan.principal)
      ~(destination : Setup_plan.context) =
    if
      principal.id = o.grantor_user_id
      && destination.room_id = Some o.channel_id
    then Ok ()
    else Error "not grantor"
  in
  let apply_ops ~plan:_ ~receipt_id:_ =
    try
      Result.map
        (fun a -> activated := Some a)
        (Store.activate_in_tx c.db ~order_id:o.id ~active_since:action_ts
           ~confirmed_by:c.clicker ~bot_user_id:v.bot_user_id ~bot_id:v.bot_id
           ~now:c.now)
    with _ -> Error "confirm_apply_exception"
  in
  Setup_plan_apply.init_schema c.db;
  match
    Setup_plan_apply.apply ~db:c.db ~plan_id ~digest
      ~principal:
        { Setup_plan.id = c.clicker; kind = Channel_actor; label = None }
      ~current_base_revision:(Setup_plan.base_revision_of_config v.config)
      ~destination_room:o.channel_id ~now:c.now ~authority ~apply_ops ()
  with
  | Setup_plan_apply.Rejected { reason = Stale_revision; _ } ->
      reply_with v.call c Render.config_changed_text
  | Rejected { reason; _ } ->
      reply_with v.call c
        (Printf.sprintf "Not confirmed (%s)."
           (Setup_plan_apply.string_of_reject_reason reason))
  | Applied { first_time = false; _ } -> ignored "the plan was already applied"
  | Applied { first_time = true; _ } ->
      let committed = Option.value !activated ~default:o in
      Lwt.catch
        (fun () ->
          let active = Option.value (Store.find c.db o.id) ~default:committed in
          use_card c;
          ledger c ~event_type:"standing_order_confirmed" active;
          let* () =
            announce_activation ~db:c.db ~call:v.call ~now:c.now active
          in
          match Store.find c.db active.id with
          | Some current
            when current.state = T.Active
                 && current.epoch = active.epoch
                 && Store.activation_announced c.db ~order_id:current.id ->
              close_card v.call c
                (Render.card_closed current (Render.Confirmed_by c.clicker))
          | _ -> Lwt.return_unit)
        (fun exn ->
          let* () =
            pause_unannounced ~db:c.db ~call:v.call ~now:c.now committed
              "confirm_post_commit_failed"
          in
          match exn with
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | _ -> Lwt.return_unit)

let confirm c =
  match (c.card.used_at, c.action_ts, c.card.plan_id, c.card.plan_digest) with
  | Some _, _, _, _ -> ignored "the confirm card was already used"
  | None, None, _, _ -> reply c (Render.no_click_time "Confirm")
  | None, Some action_ts, Some plan_id, Some digest -> (
      ignore (Store.expire_pending c.db ~now:c.now : int);
      match Store.find c.db c.order.id with
      | Some o
        when o.state = T.Pending_confirmation
             && Store.pending_open o ~now:c.now
             && card_open c -> (
          if c.clicker <> o.grantor_user_id then
            reply c
              (Printf.sprintf "Only %s can confirm this card."
                 (Render.user_ref o.grantor_user_id))
          else
            let* evaluated =
              Grant.evaluate ~db:c.db ~current_config:(current_config c)
                ~now:c.now ~stage:Grant.Confirm ~unit_label:"confirm"
                (Grant.subject_of_order o)
            in
            match evaluated with
            | Error f ->
                reply c
                  (Printf.sprintf "Not confirmed: %s."
                     (Render.reason_text f.Grant.reason))
            | Ok v -> apply_confirm c v o ~action_ts ~plan_id ~digest)
      | _ -> reply c Render.expired_text)
  | None, Some _, _, _ -> ignored "the confirm card names no plan"

(* ── Cancel (a confirm card's second button) ────────────────────────────── *)

let cancel c =
  let o = c.order in
  let n = T.display_name o in
  if Option.is_some c.card.used_at then
    ignored "the confirm card was already used"
  else if o.state <> T.Pending_confirmation then
    (* The withdrawal below re-checks this in its transaction. *)
    ignored "the order is no longer waiting for confirmation"
  else if c.clicker <> o.grantor_user_id && not (is_admin c) then
    reply c
      (Printf.sprintf "Only %s or a clawq admin can cancel %s."
         (Render.grantor o) n)
  else
    match fresh_call c with
    | Error code ->
        Logs.warn (fun m ->
            m "standing-order %s cancel click refused: %s" o.id code);
        Lwt.return_unit
    | Ok call -> (
        let* checked =
          Grant.check_member ~call ~config:(current_config c ())
            ~team_id:c.card.team_id ~channel_id:c.card.channel_id
            ~user_id:c.clicker ~require_allowlist:false
        in
        match checked with
        | Error r ->
            reply_with call c
              (Printf.sprintf "Not cancelled: %s."
                 (Render.member_refusal_text ~action:`Cancel r))
        | Ok () -> (
            match
              Store.deactivate c.db ~order_id:o.id (Store.Withdraw c.clicker)
                ~now:c.now
            with
            | Error e ->
                Logs.warn (fun m ->
                    m "standing-order %s not withdrawn: %s" o.id e);
                reply_with call c
                  (Printf.sprintf
                     "clawq could not withdraw %s; nothing changed. Try again."
                     n)
            | Ok { Store.changed = false; _ } ->
                ignored "the order is no longer waiting for confirmation"
            | Ok { Store.after; _ } ->
                use_card c;
                ledger c ~event_type:"standing_order_withdrawn" after;
                close_card call c
                  (Render.card_closed after (Render.Withdrawn_by c.clicker))))

(* ── Resume ─────────────────────────────────────────────────────────────── *)

let not_resumed text = Printf.sprintf "Not resumed: %s." text

(* Step 5: the epoch-fenced resume at the click's Slack time, then step 6. *)
let apply_resume c (v : Grant.validated) ~action_ts =
  let n = T.display_name c.order in
  ignore (Store.expire_pending c.db ~now:c.now : int);
  match
    Store.resume c.db ~order_id:c.order.id ~expected_epoch:c.card.order_epoch
      ~active_since:action_ts ~bot_user_id:v.bot_user_id ~bot_id:v.bot_id
      ~now:c.now
  with
  | Error Store.Stale_epoch ->
      reply_with v.call c
        "This resume card is out of date; ask clawq for a new one."
  | Error (Store.Duplicate_live_on_resume k) ->
      reply_with v.call c
        (not_resumed (Render.reason_text (Grant.Duplicate_order k)))
  | Error (Store.Not_resumable state) ->
      reply_with v.call c
        (not_resumed (Render.reason_text (Grant.Order_not_in_state state)))
  | Error (Store.Resume_failed e) ->
      Logs.warn (fun m -> m "standing-order %s not resumed: %s" c.order.id e);
      reply_with v.call c
        (Printf.sprintf "clawq could not resume %s; nothing changed. Try again."
           n)
  | Ok after ->
      Lwt.catch
        (fun () ->
          let* () =
            if Store.activation_announced c.db ~order_id:after.id then
              Lwt.return_unit
            else announce_activation ~db:c.db ~call:v.call ~now:c.now after
          in
          (* A failed or ambiguous notice paused the order. Never claim a
             successful resume or adopt its still-unbound Pause button. *)
          match Store.find c.db after.id with
          | Some current
            when current.state = T.Active
                 && current.epoch = after.epoch
                 && Store.activation_announced c.db ~order_id:current.id ->
              use_card c;
              ledger c ~event_type:"standing_order_resumed" after;
              let* () =
                close_card v.call c
                  (Render.card_closed after (Render.Resumed_by c.clicker))
              in
              if c.clicker = after.grantor_user_id then Lwt.return_unit
              else
                post v.call ~what:"resume_notice"
                  ~channel_id:after.management_channel_id
                  ~thread_ts:after.management_thread_ts
                  (Render.grantor_notice after
                     (Render.Resumed_by_user c.clicker))
          | _ -> Lwt.return_unit)
        (fun exn ->
          let* () =
            pause_unannounced ~db:c.db ~call:v.call ~now:c.now after
              "resume_post_commit_failed"
          in
          match exn with
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | _ -> Lwt.return_unit)

(* The capability gate runs before any HTTP, so a denied capability costs only
   the refusal (ruling F2); a non-grantor admin then needs the allowlist and
   channel membership; then the full preflights (spec §6.8). *)
let resume c =
  let o = c.order in
  match (c.card.used_at, c.action_ts) with
  | Some _, _ -> ignored "the resume card was already used"
  | None, _ when not (card_open c) -> reply c Render.expired_text
  | None, None -> reply c (Render.no_click_time "Resume")
  | None, Some action_ts -> (
      let by_grantor = c.clicker = o.grantor_user_id in
      if (not by_grantor) && not (is_admin c) then
        reply c
          (Printf.sprintf "Only %s or a clawq admin can resume %s."
             (Render.grantor o) (T.display_name o))
      else
        let config = current_config c () in
        match
          Grant.gate ~db:c.db ~config ~channel_id:c.card.channel_id
            ~order_ref:(Pause.order_ref o)
        with
        | Error f -> reply c (not_resumed (Render.reason_text f.Grant.reason))
        | Ok (_, snapshot) -> (
            let call = make_call c snapshot in
            let* member =
              if by_grantor then Lwt.return (Ok ())
              else
                Grant.check_member ~call ~config ~team_id:c.card.team_id
                  ~channel_id:c.card.channel_id ~user_id:c.clicker
                  ~require_allowlist:true
            in
            match member with
            | Error r ->
                reply_with call c
                  (not_resumed (Render.member_refusal_text ~action:`Resume r))
            | Ok () -> (
                ignore (Store.expire_pending c.db ~now:c.now : int);
                let* evaluated =
                  Grant.evaluate ~db:c.db ~current_config:(current_config c)
                    ~now:c.now ~stage:Grant.Resume ~unit_label:"resume"
                    (Grant.subject_of_order o)
                in
                match evaluated with
                | Error f ->
                    reply_with call c
                      (not_resumed (Render.reason_text f.Grant.reason))
                | Ok v -> apply_resume c v ~action_ts)))

(* ── Pause (never expires, not single-use) ──────────────────────────────── *)

let pause c =
  let o = c.order in
  let* outcome =
    Pause.run ~manager:c.manager ~now:c.now ~team_id:c.card.team_id
      ~app_id:c.card.app_id ~channel_id:c.card.channel_id ~user_id:c.clicker
      ~target:(`Order o.id) ~source:Pause.Button ()
  in
  let outcome =
    match outcome with
    | Pause.Not_found _ -> Pause.Not_found o.display_no
    | outcome -> outcome
  in
  (* An unknown diagnostic count still gets the successful pause receipt. *)
  Pause.post_receipt ~manager:c.manager ~team_id:c.card.team_id
    ~app_id:c.card.app_id
    ?order:(Pause.order_of_outcome outcome)
    ~channel_id:c.card.channel_id ~thread_ts:(reply_thread c)
    (Pause.receipt ~display_no:o.display_no ~by:c.clicker outcome)

(* ── Entry point ────────────────────────────────────────────────────────── *)

let handle_action ~manager ?now ~payload action =
  let now = match now with Some t -> t | None -> Store.now () in
  Lwt.catch
    (fun () ->
      match
        (button_of_action_id (field "action_id" action), Session.get_db manager)
      with
      | None, _ -> ignored "not a standing-order button"
      | Some _, None -> ignored "clawq has no database"
      | Some button, Some db -> (
          match bind ~manager ~db ~now ~payload ~button action with
          | Error why -> ignored why
          | Ok c -> (
              match (button, c.card.kind) with
              | Press_confirm, _ -> confirm c
              | Press_cancel, T.Change_card ->
                  Change.handle_cancel ~manager ~now ~card:c.card
                    ~clicker:c.clicker
              | Press_cancel, _ -> cancel c
              | Press_resume, _ -> resume c
              | Press_pause, _ -> pause c
              | Press_change, _ ->
                  Change.handle_click ~manager ~now
                    ~announce:announce_activation ~pause_unannounced
                    ~card:c.card ~clicker:c.clicker ~action_ts:c.action_ts)))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn ->
          Logs.warn (fun m ->
              m "standing-order card click failed: %s" (Printexc.to_string exn));
          Lwt.return_unit)
