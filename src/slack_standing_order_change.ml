(** Change a standing order's instruction (spec §6.8, Decision 14): the
    [slack_standing_order_change] tool and its bound change card.

    The tool writes no pending row (it would collide with the live one on the
    partial unique index): it stores a [Setup_plan] carrying the draft and posts
    a change card bound to that plan. Only the requester's click applies it, and
    only while the requester is the old order's grantor or a clawq admin. The
    click re-runs the full plan preflights, then [Setup_plan_apply.apply]
    cancels the old order and inserts its successor as active, with the next
    [SO-n], in one transaction. The clicker becomes the successor's grantor and
    its daily cap starts fresh; target and channel never change. Natural
    language never changes an order. *)

open Lwt.Syntax
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Render = Slack_standing_order_render
module Pause = Slack_standing_order_pause
module Tools = Slack_standing_order_tools

let tool_name = "slack_standing_order_change"
let apply_kind = "slack_standing_order_change"
let error = Tools.error
let order_path id = "slack_standing_orders/" ^ id

(* ── The draft a change plan carries ────────────────────────────────────── *)

type draft = {
  replaces : string;
  order_id : string;  (** The successor's opaque id, fixed at plan time. *)
  action : T.action;
  management_thread_ts : string;
  principal_id : string option;
}

let draft_to_json d : Yojson.Safe.t =
  `Assoc
    [
      ("replaces", `String d.replaces);
      ("order_id", `String d.order_id);
      ("action", T.action_to_json d.action);
      ("management_thread_ts", `String d.management_thread_ts);
      ( "principal_id",
        Option.fold ~none:`Null ~some:(fun p -> `String p) d.principal_id );
    ]

let draft_of_plan (plan : Setup_plan.t) =
  let data = plan.apply_payload.data in
  let text key =
    match Slack_huddles.get key data with
    | `String s when s <> "" -> Some s
    | _ -> None
  in
  match
    ( plan.apply_payload.kind,
      text "replaces",
      text "order_id",
      text "management_thread_ts" )
  with
  | Setup_plan.Generic kind, Some replaces, Some order_id, Some thread_ts
    when kind = apply_kind -> (
      match T.action_of_json (Slack_huddles.get "action" data) with
      | Ok action ->
          Some
            {
              replaces;
              order_id;
              action;
              management_thread_ts = thread_ts;
              principal_id = text "principal_id";
            }
      | Error _ -> None)
  | _ -> None

(* ── The tool ───────────────────────────────────────────────────────────── *)

let not_owner (o : T.order) =
  Printf.sprintf "Only %s or a clawq admin can change %s." (Render.grantor o)
    (T.display_name o)

let state_refusal (o : T.order) =
  let n = T.display_name o in
  match o.state with
  | T.Pending_confirmation ->
      Some
        (Printf.sprintf
           "%s is still waiting for confirmation; cancel it and create a new \
            one."
           n)
  | T.Cancelled ->
      Some
        (Printf.sprintf "%s was cancelled; create a new standing order instead."
           n)
  | T.Active | T.Paused | T.Suspended -> None

(* [when_no_transcript] defaults to the old order's. *)
let change_args json (old : T.order) =
  let (T.Relay_to_app_mention { when_no_transcript = current; _ }) =
    old.action
  in
  let when_no_transcript =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "when_no_transcript" fields with
        | None | Some `Null -> Some current
        | Some (`String s) -> T.when_no_transcript_of_string s
        | Some _ -> None)
    | _ -> None
  in
  match (Tools.string_arg "instruction" json, when_no_transcript) with
  | Some instruction, Some wnt -> Ok (instruction, wnt)
  | _ ->
      Error
        (error
           "\"instruction\" is a required string, and \"when_no_transcript\" \
            is \"relay_notes\" or \"skip\" (left out, the order keeps its \
            current setting); nothing changed.")

let store_plan (p : Tools.prelude) (v : Grant.validated) ~(old : T.order)
    (d : draft) =
  let context session_key : Setup_plan.context =
    {
      room_id = Some p.route.channel;
      session_key;
      connector = Some "slack";
      profile_id = v.snapshot.profile_id;
      extra = [];
    }
  in
  let old_json = T.action_to_json old.action
  and new_json = T.action_to_json d.action in
  let plan =
    Setup_plan.make
      ~principal:
        {
          Setup_plan.id = Tools.requester p;
          kind = Channel_actor;
          label = None;
        }
      ~source:(context (Some p.turn.request.key))
      ~destination:(context None) ~current_state:old_json
      ~planned_state:new_json
      ~diff:
        [
          Setup_plan.Delete { path = order_path old.id; old = old_json };
          Setup_plan.Create { path = order_path d.order_id; value = new_json };
        ]
      ~readiness:
        [
          {
            Setup_plan.name = "preflight";
            status = Pass;
            message = "grant authority validated";
          };
        ]
      ~warnings:[]
      ~base_revision:(Setup_plan.base_revision_of_config v.config)
      ~apply_payload:
        {
          Setup_plan.kind = Generic apply_kind;
          ops = `List [];
          data = draft_to_json d;
        }
      ~ttl_seconds:Tools.card_ttl ~now:p.now ()
  in
  Setup_plan_apply.init_schema p.db;
  Result.map (fun () -> plan) (Setup_plan_apply.store_plan ~db:p.db plan)

(* Step 7: the card is bound to the old order at the epoch the evaluation read
   last, so any later state change makes it out of date. *)
let post_change_card (p : Tools.prelude) (v : Grant.validated) ~(old : T.order)
    (d : draft) (plan : Setup_plan.t) =
  let user = Tools.requester p and n = T.display_name old in
  let card =
    Store.insert_card p.db ~kind:T.Change_card ~order:old ~created_by:user
      ~reply_thread_ts:p.route.root ~plan_id:plan.id ~plan_digest:plan.digest
      ~expires_at:(p.now +. Tools.card_ttl) ~now:p.now ()
  in
  let+ posted =
    Tools.post_card v.call ~channel_id:p.route.channel ~thread_ts:p.route.root
      (Render.change_card ~replaces:old ~grantor:user ~action:d.action
         ~card_id:card.id)
  in
  match posted with
  | Error code ->
      (* An unposted card can't be clicked; close it anyway. *)
      ignore (Store.use_card p.db ~card_id:card.id ~now:p.now : bool);
      Logs.warn (fun m ->
          m "standing-order %s change card not posted: %s" old.id code);
      error "clawq could not post the change card; nothing changed."
  | Ok ts ->
      Store.set_card_message_ts p.db ~card_id:card.id ts;
      Pause.ledger ~db:p.db ~channel_id:p.route.channel ~actor:user
        ~event_type:"standing_order_change_planned"
        [
          ("order", `String n);
          ("target_app_id", `String (T.action_target d.action).app_id);
        ];
      Printf.sprintf
        "A change card for %s was posted in this thread. %s is unchanged until \
         %s presses Confirm change (expires in 10 minutes)."
        n n (Render.user_ref user)

(* Steps 2-7: authority, state, the instruction, the plan evaluation (the gate
   inside it keeps a capability deny at zero HTTP), the plan and the card. *)
let change ~current_config (p : Tools.prelude) json (old : T.order) =
  let user = Tools.requester p in
  let admin =
    p.turn.is_admin && Admin.is_admin ~db:p.db ~channel:"slack" ~sender_id:user
  in
  if user <> old.grantor_user_id && not admin then Lwt.return (not_owner old)
  else
    match state_refusal old with
    | Some text -> Lwt.return text
    | None -> (
        match change_args json old with
        | Error text -> Lwt.return text
        | Ok (raw, when_no_transcript) -> (
            match T.validate_instruction raw with
            | Error reason -> Lwt.return (Tools.instruction_error reason)
            | Ok instruction -> (
                let order_id = Store.new_id ~prefix:"so" in
                let target = T.action_target old.action in
                let subject : Grant.subject =
                  {
                    order_id;
                    team_id = p.route.team;
                    enterprise_id = old.enterprise_id;
                    api_app_id = p.route.app;
                    channel_id = p.route.channel;
                    grantor_user_id = user;
                    target;
                  }
                in
                let* evaluated =
                  Grant.evaluate ~db:p.db ~current_config ~now:p.now
                    ~stage:(Grant.Plan { replaces = Some old.id })
                    ~unit_label:"plan" subject
                in
                match evaluated with
                | Error f ->
                    Lwt.return (error (Render.reason_text f.Grant.reason))
                | Ok (v : Grant.validated) -> (
                    let old = Option.value v.replaced ~default:old in
                    let draft =
                      {
                        replaces = old.id;
                        order_id;
                        action =
                          T.Relay_to_app_mention
                            { target; instruction; when_no_transcript };
                        management_thread_ts = p.route.root;
                        principal_id =
                          Tools.principal_for ~db:p.db ~team_id:p.route.team
                            ~user_id:user;
                      }
                    in
                    match store_plan p v ~old draft with
                    | Error e ->
                        Logs.warn (fun m ->
                            m "standing-order %s change plan not stored: %s"
                              old.id e);
                        Lwt.return
                          (error
                             "clawq could not record the change plan; nothing \
                              changed.")
                    | Ok plan -> post_change_card p v ~old draft plan))))

let schema =
  Tools.object_schema ~required:[ "order"; "instruction" ]
    [
      ( "order",
        Tools.string_schema
          "The order's name in this channel, for example SO-3." );
      ( "instruction",
        Tools.string_schema
          "The new instruction for the target bot, at most 1000 characters. It \
           replaces the old one and is shown in full on the change card." );
      ( "when_no_transcript",
        `Assoc
          [
            ("type", `String "string");
            ("enum", `List [ `String "relay_notes"; `String "skip" ]);
            ( "description",
              `String
                "relay_notes: relay the AI notes alone, labelled as such; \
                 skip: relay nothing for that huddle. Left out, the order \
                 keeps its current setting." );
          ] );
    ]

let spec ~current_config : Tools.spec =
  {
    name = tool_name;
    description =
      "Change the instruction of one of this Slack channel's standing orders. \
       Posts a change card in this thread; nothing changes until the requester \
       presses Confirm change, which cancels the order and starts a new one \
       (the next SO number) with the new instruction, the same target, and the \
       requester as its owner. Only the order's owner or a clawq admin can \
       change it.";
    schema;
    args = [ "order"; "instruction"; "when_no_transcript" ];
    risk = Tool.Medium;
    run =
      (fun p json ->
        Tools.with_order (fun p o -> change ~current_config p json o) p json);
  }

(* Stable registration, like the other management tools. *)
let register ~current_config registry =
  Tool_registry.replace registry
    (Tools.tool ~current_config (spec ~current_config))

(* ── Card clicks ────────────────────────────────────────────────────────── *)

let expired_text = "This card has expired; ask clawq again for a new one."

let no_click_time =
  "Slack did not include a click time; press Confirm change again."

let out_of_date_text =
  "This change card is out of date; ask clawq for a new one."

let config_changed_text =
  "clawq's configuration changed since this card was made; ask again to get a \
   new card."

type click = {
  manager : Session.t;
  db : Sqlite3.db;
  now : float;
  card : T.card;
  old : T.order;  (** The replaced order as it was when the click arrived. *)
  draft : draft;
  clicker : string;
  message_ts : string;  (** The card's own message. *)
}

(* [Slack_standing_order_cards.announce_activation], passed in so this module
   stays below Cards. *)
type announce =
  db:Sqlite3.db -> call:SO.call -> now:float -> T.order -> unit Lwt.t

let ignored why =
  Logs.info (fun m -> m "standing-order change click ignored: %s" why);
  Lwt.return_unit

(* Cards has bound the click to this card; this re-reads the card's order and
   its plan's draft, which must name that order. *)
let bind ~manager ~now ~(card : T.card) ~clicker =
  match (Session.get_db manager, card.kind, card.message_ts) with
  | None, _, _ -> Error "clawq has no database"
  | Some _, kind, _ when kind <> T.Change_card -> Error "not a change card"
  | Some _, _, None -> Error "the change card was never posted"
  | Some db, _, Some message_ts -> (
      match Store.find db card.order_id with
      | Some (old : T.order)
        when old.team_id = card.team_id
             && old.api_app_id = card.app_id
             && old.channel_id = card.channel_id -> (
          let draft =
            Option.bind card.plan_id (fun plan_id ->
                Option.bind
                  (Setup_plan_apply.get_plan ~db ~plan_id)
                  draft_of_plan)
          in
          match draft with
          | Some d when d.replaces = old.id ->
              Ok { manager; db; now; card; old; draft = d; clicker; message_ts }
          | _ -> Error "the change card names no readable plan")
      | _ -> Error "the card's order is unavailable")

let reply_thread c = Option.value c.card.reply_thread_ts ~default:c.message_ts

(* A best-effort reply in the card's thread under a fresh snapshot. *)
let reply c text =
  Pause.post ~manager:c.manager ~order:c.old ~unit_label:"change_reply"
    ~channel_id:c.card.channel_id ~thread_ts:(reply_thread c) text

let current_config c () = Session.get_config c.manager
let is_admin c = Admin.is_admin ~db:c.db ~channel:"slack" ~sender_id:c.clicker

(* Replaces the card's blocks, so its buttons disappear. *)
let close_card (call : SO.call) c (closed : Render.rendered) =
  let warn code =
    Logs.warn (fun m ->
        m "standing-order %s change card not closed: %s" c.old.id code)
  in
  Lwt.catch
    (fun () ->
      let+ result =
        call "chat.update"
          [
            ("channel", `String c.card.channel_id);
            ("ts", `String c.message_ts);
            ("text", `String closed.text);
            ("blocks", closed.blocks);
          ]
      in
      match result with Ok _ -> () | Error e -> warn (SO.error_code e))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn ->
          warn ("exception: " ^ Printexc.to_string exn);
          Lwt.return_unit)

(* Never raises: after the commit, a failure here must not skip the
   activation notice. *)
let use_card c =
  try Store.use_card c.db ~card_id:c.card.id ~now:c.now
  with exn ->
    Logs.warn (fun m ->
        m "standing-order change card %s not marked used: %s" c.card.id
          (Printexc.to_string exn));
    false

let applied c = Option.is_some (Store.find c.db c.draft.order_id)

(* Step 5 after the commit: close the card, announce the successor (whose
   notice failure pauses it), then tell the old grantor when someone else
   changed their order. *)
let changed c (v : Grant.validated) ~(announce : announce)
    ((old_after : T.order), (fresh : T.order)) =
  ignore (use_card c : bool);
  Pause.ledger ~db:c.db ~channel_id:c.card.channel_id ~actor:c.clicker
    ~event_type:"standing_order_changed"
    [
      ("from", `String (T.display_name old_after));
      ("to", `String (T.display_name fresh));
    ];
  let new_display_no = fresh.display_no in
  let* () =
    close_card v.call c
      (Render.card_closed old_after
         (Render.Changed_by { by = c.clicker; new_display_no }))
  in
  let* () = announce ~db:c.db ~call:v.call ~now:c.now fresh in
  if c.clicker = old_after.grantor_user_id then Lwt.return_unit
  else
    Pause.post ~manager:c.manager ~order:old_after ~unit_label:"change_notice"
      ~channel_id:old_after.management_channel_id
      ~thread_ts:old_after.management_thread_ts
      (Render.grantor_notice old_after
         (Render.Replaced_by { by = c.clicker; new_display_no }))

(* Step 4: the compare-and-swap on plan id and digest, under the config
   revision the plan was made with, around [Store.replace_in_tx]. *)
let apply_change c (v : Grant.validated) ~announce ~action_ts ~plan_id ~digest =
  let replaced = ref None in
  let authority ~(principal : Setup_plan.principal)
      ~(destination : Setup_plan.context) =
    if principal.id = c.clicker && destination.room_id = Some c.card.channel_id
    then Ok ()
    else Error "not the requester"
  in
  let apply_ops ~plan:_ ~receipt_id:_ =
    Result.map
      (fun pair -> replaced := Some pair)
      (Store.replace_in_tx c.db ~old_id:c.old.id
         ~expected_epoch:c.card.order_epoch ~new_id:c.draft.order_id
         ~grantor_user_id:c.clicker ?principal_id:c.draft.principal_id
         ~action:c.draft.action
         ~management_thread_ts:c.draft.management_thread_ts
         ~active_since:action_ts ~confirmed_by:c.clicker
         ~bot_user_id:v.bot_user_id ~bot_id:v.bot_id ~now:c.now ())
  in
  Setup_plan_apply.init_schema c.db;
  match
    Setup_plan_apply.apply ~db:c.db ~plan_id ~digest
      ~principal:
        { Setup_plan.id = c.clicker; kind = Channel_actor; label = None }
      ~current_base_revision:(Setup_plan.base_revision_of_config v.config)
      ~destination_room:c.card.channel_id ~now:c.now ~authority ~apply_ops ()
  with
  | Setup_plan_apply.Rejected { reason = Stale_revision; _ } ->
      reply c config_changed_text
  | Rejected { reason; _ } ->
      reply c
        (Printf.sprintf "Not changed (%s)."
           (Setup_plan_apply.string_of_reject_reason reason))
  | Applied { first_time = false; _ } ->
      ignored "the change plan was already applied"
  | Applied { first_time = true; _ } -> (
      match !replaced with
      | Some pair -> changed c v ~announce pair
      | None ->
          Logs.err (fun m ->
              m "standing-order %s change applied without a successor" c.old.id);
          Lwt.return_unit)

(* Steps 1-3: a used or already-applied card is ignored; expiry, the click
   time and authority are checked before any HTTP; then the full preflights
   at the [Replace] stage. *)
let handle_click ~manager ~now ~announce ~card ~clicker ~action_ts =
  match bind ~manager ~now ~card ~clicker with
  | Error why -> ignored why
  | Ok c -> (
      let action_ts =
        Option.bind action_ts (fun ts ->
            if T.valid_slack_ts ts then Some ts else None)
      in
      match (c.card.used_at, c.card.plan_id, c.card.plan_digest) with
      | Some _, _, _ -> ignored "the change card was already used"
      | None, Some plan_id, Some digest -> (
          if applied c then ignored "the change was already applied"
          else
            match c.card.expires_at with
            | Some t when t > now -> (
                match action_ts with
                | None -> reply c no_click_time
                | Some action_ts ->
                    if clicker <> c.card.created_by then
                      reply c
                        (Printf.sprintf "Only %s can confirm this card."
                           (Render.user_ref c.card.created_by))
                    else if clicker <> c.old.grantor_user_id && not (is_admin c)
                    then reply c (not_owner c.old)
                    else (
                      ignore (Store.expire_pending c.db ~now : int);
                      let subject : Grant.subject =
                        {
                          order_id = c.draft.order_id;
                          team_id = c.card.team_id;
                          enterprise_id = c.old.enterprise_id;
                          api_app_id = c.card.app_id;
                          channel_id = c.card.channel_id;
                          grantor_user_id = clicker;
                          target = T.action_target c.old.action;
                        }
                      in
                      let* evaluated =
                        Grant.evaluate ~db:c.db
                          ~current_config:(current_config c) ~now
                          ~stage:
                            (Grant.Replace
                               {
                                 replaces = c.old.id;
                                 replaced_epoch = c.card.order_epoch;
                               })
                          ~unit_label:"change" subject
                      in
                      match evaluated with
                      | Error { Grant.reason = Grant.Epoch_mismatch; _ } ->
                          reply c out_of_date_text
                      | Error f ->
                          reply c
                            (Printf.sprintf "Not changed: %s."
                               (Render.reason_text f.Grant.reason))
                      | Ok v ->
                          apply_change c v ~announce ~action_ts ~plan_id ~digest)
                )
            | _ -> reply c expired_text)
      | None, _, _ -> ignored "the change card names no plan")

(* Cancel changes no order, so it needs no membership check (ruling F4). *)
let handle_cancel ~manager ~now ~card ~clicker =
  match bind ~manager ~now ~card ~clicker with
  | Error why -> ignored why
  | Ok c -> (
      if Option.is_some c.card.used_at then
        ignored "the change card was already used"
      else if applied c then ignored "the change was already applied"
      else if clicker <> c.card.created_by && not (is_admin c) then
        reply c
          (Printf.sprintf "Only %s or a clawq admin can cancel this change."
             (Render.user_ref c.card.created_by))
      else
        match
          SO.snapshot ~db:c.db ~config:(current_config c ())
            ~channel_id:c.card.channel_id ~order_ref:(Pause.order_ref c.old)
        with
        | Error code ->
            Logs.warn (fun m ->
                m "standing-order %s change cancel refused: %s" c.old.id code);
            Lwt.return_unit
        | Ok snapshot ->
            if use_card c then
              let call =
                SO.make ~db:c.db ~current_config:(current_config c) ~snapshot
                  ~channel_id:c.card.channel_id
                  ~audit:
                    (SO.audit_label ~order_id:(Some c.old.id)
                       ~unit_label:"change")
                  ()
              in
              close_card call c
                (Render.card_closed c.old (Render.Change_cancelled_by clicker))
            else ignored "the change card was already used")
