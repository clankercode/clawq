(** Operator CLI for Slack standing orders (spec §6.8, CLI parity):
    [clawq slack orders list|show|fires|pause|resume|cancel].

    Reads are open. Mutations need [CLAWQ_ADMIN=1] (or [true]); each denial is
    audited against the order's room. Pause and cancel are an operator kill
    switch: a state change, a receipt and a ledger row with actor [cli], and
    nothing posted to Slack (Decision 12). Resume re-runs the full Resume
    preflight over direct Slack HTTP. There is no create or change here: both
    need verified Slack provenance (§6.8, ADR 0005). *)

module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module Grant = Slack_standing_order_grant
module Render = Slack_standing_order_render
module Pause = Slack_standing_order_pause
module R = Slack_standing_order_recovery_store

let actor = "cli"
let default_fire_limit = 20

let usage =
  String.concat "\n"
    [
      "Usage: clawq slack orders <subcommand>";
      "";
      "Subcommands:";
      "  list [--channel C]    List standing orders, cancelled ones included.";
      "  show ID               Show one order. Its instruction is untrusted \
       text.";
      "  fires ID [--limit N]  Show an order's latest fires, newest first \
       (default 20).";
      "  pause ID              Pause an active order or withdraw a pending one \
       (CLAWQ_ADMIN=1).";
      "  resume ID             Re-check a paused or suspended order with Slack \
       and resume it (CLAWQ_ADMIN=1).";
      "  cancel ID             Cancel an order (CLAWQ_ADMIN=1).";
      "";
      "ID is the opaque order id that list shows. Pause, resume and cancel \
       post nothing to Slack.";
      "Orders are created and changed only in Slack, because both need a \
       verified Slack request.";
    ]

type mutation = Pause_order | Resume_order | Cancel_order

let mutation_name = function
  | Pause_order -> "pause"
  | Resume_order -> "resume"
  | Cancel_order -> "cancel"

type request =
  | List of string option
  | Show of string
  | Fires of { id : string; limit : int }
  | Mutate of mutation * string

let parse = function
  | [ "list" ] -> Ok (List None)
  | [ "list"; "--channel"; channel ] -> Ok (List (Some channel))
  | [ "show"; id ] -> Ok (Show id)
  | [ "fires"; id ] -> Ok (Fires { id; limit = default_fire_limit })
  | [ "fires"; id; "--limit"; n ] -> (
      match int_of_string_opt n with
      | Some limit when limit > 0 -> Ok (Fires { id; limit })
      | _ -> Error "Error: --limit must be a positive whole number.")
  | [ "pause"; id ] -> Ok (Mutate (Pause_order, id))
  | [ "resume"; id ] -> Ok (Mutate (Resume_order, id))
  | [ "cancel"; id ] -> Ok (Mutate (Cancel_order, id))
  | _ -> Error usage

(* ── Reads ──────────────────────────────────────────────────────────────── *)

let dash = Option.value ~default:"-"
let target_label (o : T.order) = (T.action_target o.action).label

let last_fire db (o : T.order) =
  match
    Slack_standing_order_recovery_store.list_fires_readonly db ~order_id:o.id
      ~limit:1
  with
  | (f : T.fire) :: _ ->
      Printf.sprintf "%s in %s (%s)"
        (T.fire_state_to_string f.state)
        f.room_id f.created_at
  | [] -> "-"

let column header =
  {
    Table_format.header;
    align = Table_format.Left;
    min_width = 2;
    flex = false;
  }

(* A fixed width keeps Table_format from probing the terminal. *)
let table columns rows =
  Table_format.render ~max_width:200 (List.map column columns) rows

let cmd_list db channel_id =
  match Store.list db ?channel_id ~include_cancelled:true () with
  | [] -> (
      match channel_id with
      | None -> "No standing orders."
      | Some c -> Printf.sprintf "No standing orders in channel %s." c)
  | orders ->
      "Standing orders:\n"
      ^ table
          [ "ID"; "CHANNEL"; "SO"; "STATE"; "TARGET"; "GRANTOR"; "LAST FIRE" ]
          (List.map
             (fun (o : T.order) ->
               [
                 o.id;
                 o.channel_id;
                 T.display_name o;
                 T.state_to_string o.state;
                 target_label o;
                 o.grantor_user_id;
                 last_fire db o;
               ])
             orders)

(* Continuation lines are indented, so an instruction can't pass for a field. *)
let indent_continuation text =
  String.concat "\n  " (String.split_on_char '\n' text)

let cmd_show db (o : T.order) =
  let (T.Relay_to_app_mention { target; instruction; when_no_transcript }) =
    o.action
  in
  let field key value = key ^ ": " ^ value in
  String.concat "\n"
    (List.concat
       [
         [
           Printf.sprintf "Standing order %s (%s)" (T.display_name o) o.id;
           field "State" (T.state_to_string o.state);
           field "Recovery readiness"
             (R.readiness_name (R.rollout_readiness db ~order_id:o.id));
           field "Already delivered (external)"
             (string_of_int (R.suppression_count db ~order_id:o.id));
           field "Recovery metadata"
             (Yojson.Safe.to_string (R.status_json db ~order_id:o.id));
           field "Team" o.team_id;
         ];
         Option.fold ~none:[]
           ~some:(fun e -> [ field "Enterprise" e ])
           o.enterprise_id;
         [
           field "App" o.api_app_id;
           field "Channel" o.channel_id;
           field "Grantor" o.grantor_user_id;
           field "Target"
             (Printf.sprintf "%s (app %s, user %s, bot %s)" target.label
                target.app_id target.user_id target.bot_id);
           field "Trigger" (T.trigger_to_string o.trigger);
           field "When no transcript"
             (T.when_no_transcript_to_string when_no_transcript);
           field "Relays a day (max)" (string_of_int o.max_fires_per_day);
           field "Epoch" (string_of_int o.epoch);
           field "Card version" (string_of_int o.card_version);
           field "Created" o.created_at;
         ];
         Option.fold ~none:[]
           ~some:(fun t ->
             [ field "Pending until" (Time_util.sql_datetime_utc ~t ()) ])
           o.pending_expires_at;
         [
           field "Confirmed"
             (match (o.confirmed_at, o.confirmed_by) with
             | Some at, Some by -> Printf.sprintf "%s by %s" at by
             | Some at, None -> at
             | None, _ -> "-");
           field "Active since" (dash o.active_since);
           field "Paused by" (dash o.paused_by);
           field "Suspended reason" (dash o.suspended_reason);
           field "Cancelled by" (dash o.cancelled_by);
           field "Management thread"
             (Printf.sprintf "%s %s" o.management_channel_id
                o.management_thread_ts);
           field "Last fire" (last_fire db o);
           field "Instruction (untrusted)" (indent_continuation instruction);
         ];
         List.map
           (fun (cause, status) -> field "Report" (cause ^ " " ^ status))
           (R.report_statuses db ~order_id:o.id);
       ])

let cmd_fires db (o : T.order) ~limit =
  let recovery =
    "Recovery readiness: "
    ^ R.readiness_name (R.rollout_readiness db ~order_id:o.id)
    ^ "\nExternal deliveries (already_delivered): "
    ^ Yojson.Safe.to_string (`List (R.external_statuses db ~order_id:o.id))
    ^ "\n"
  in
  recovery
  ^
  match
    Slack_standing_order_recovery_store.list_fires_readonly db ~order_id:o.id
      ~limit
  with
  | [] ->
      Printf.sprintf "No fires recorded for %s (%s)." (T.display_name o) o.id
  | fires ->
      Printf.sprintf "Fires for %s (%s), newest first:\n" (T.display_name o)
        o.id
      ^ table
          [
            "FIRE"; "CREATED"; "ROOM"; "DATE START"; "STATE"; "EPOCH"; "REASON";
          ]
          (List.map
             (fun (f : T.fire) ->
               [
                 f.id;
                 f.created_at;
                 f.room_id;
                 f.date_start;
                 T.fire_state_to_string f.state;
                 string_of_int f.epoch;
                 dash f.reason;
               ])
             fires)

(* ── Mutations ──────────────────────────────────────────────────────────── *)

let ledger db (o : T.order) event_type =
  Pause.ledger ~db ~channel_id:o.channel_id ~actor ~event_type
    [
      ("order", `String (T.display_name o));
      ("source", `String (Pause.source_to_string Pause.Cli));
    ]

let store_failed what (o : T.order) reason =
  Logs.warn (fun m ->
      m "standing-order %s %s from the CLI failed: %s" o.id what reason);
  Printf.sprintf "Error: clawq could not %s %s; nothing changed. Try again."
    what (T.display_name o)

(* Ruling F9: the outcome [Pause.run] would give, including an unknown
   diagnostic count after a successful pause, so the receipt is the same. *)
let cmd_pause db ~now (o : T.order) =
  let settled (o : T.order) =
    match o.state with
    | T.Paused -> Pause.Already_paused o
    | _ -> Pause.Not_active o
  in
  let receipt outcome =
    Pause.receipt ~display_no:o.display_no ~by:actor outcome
  in
  let op =
    match o.state with
    | T.Pending_confirmation -> Some (Store.Withdraw actor)
    | T.Active -> Some (Store.Pause actor)
    | T.Paused | T.Suspended | T.Cancelled -> None
  in
  match op with
  | None -> receipt (settled o)
  | Some op -> (
      match Store.deactivate db ~order_id:o.id op ~now with
      | Error reason -> store_failed "pause" o reason
      | Ok { Store.changed = false; after; _ } -> receipt (settled after)
      | Ok { Store.after; in_flight; _ } -> (
          match op with
          | Store.Withdraw _ ->
              ledger db after "standing_order_withdrawn";
              receipt (Pause.Withdrawn after)
          | _ ->
              ledger db after "standing_order_paused";
              receipt (Pause.Paused { order = after; in_flight })))

let cmd_resume db ~config ?http ~now (o : T.order) =
  if http = None && Lwt.get Slack_transport.key = None then
    ignore (Slack_standing_order_slack.ensure_limiter ());
  let n = T.display_name o in
  let not_resumed reason =
    Printf.sprintf "Not resumed: %s." (Render.reason_text reason)
  in
  match o.state with
  | (T.Paused | T.Suspended)
    when not (Store.activation_announced db ~order_id:o.id) ->
      Printf.sprintf
        "Not resumed: resume it from Slack so the channel gets its activation \
         notice (%s)."
        n
  | T.Paused | T.Suspended -> (
      (* The daemon migrates this table at startup; a CLI run against a store
         it hasn't migrated yet would otherwise fail every snapshot. *)
      (try Access_snapshot.init_schema db
       with exn ->
         Logs.warn (fun m ->
             m "standing-order CLI: access snapshot schema: %s"
               (Printexc.to_string exn)));
      ignore (Store.expire_pending db ~now : int);
      match
        Lwt_main.run
          (Grant.evaluate ~db
             ~current_config:(fun () -> config)
             ?http ~now ~stage:Grant.Resume ~unit_label:"cli_resume"
             (Grant.subject_of_order o))
      with
      | Error f -> not_resumed f.Grant.reason
      | Ok v -> (
          ignore (Store.expire_pending db ~now : int);
          match
            Store.resume db ~order_id:o.id ~expected_epoch:o.epoch
              ~active_since:(Store.ts_of_unix now) ~bot_user_id:v.bot_user_id
              ~bot_id:v.bot_id ~now
          with
          | Ok after ->
              ledger db after "standing_order_resumed";
              Printf.sprintf
                "Standing order %s resumed (cli). Huddles that became ready \
                 while it was paused will not be relayed."
                (T.display_name after)
          | Error Store.Stale_epoch -> not_resumed Grant.Epoch_mismatch
          | Error (Store.Duplicate_live_on_resume k) ->
              not_resumed (Grant.Duplicate_order k)
          | Error (Store.Not_resumable state) ->
              not_resumed (Grant.Order_not_in_state state)
          | Error (Store.Resume_failed reason) -> store_failed "resume" o reason
          ))
  | state ->
      Printf.sprintf "Standing order %s %s; there is nothing to resume." n
        (Render.state_phrase state)

let cmd_cancel db ~now (o : T.order) =
  match Store.deactivate db ~order_id:o.id (Store.Cancel actor) ~now with
  | Error reason -> store_failed "cancel" o reason
  | Ok { Store.changed = false; _ } ->
      Printf.sprintf "Standing order %s was already cancelled; nothing changed."
        (T.display_name o)
  | Ok { Store.after; _ } ->
      ledger db after "standing_order_cancelled";
      Render.cancel_receipt after ~by:actor

(* The denial is audited against the order's room, so the lookup comes first. *)
let cmd_mutate db ~config ?http ~now mutation (o : T.order) =
  let name = mutation_name mutation in
  match
    Command_bridge_room_common.require_admin_audited
      ~room_id:("slack:" ^ o.channel_id) ~action:("standing_order_" ^ name)
  with
  | Some denied -> denied
  | None -> (
      match mutation with
      | Pause_order -> cmd_pause db ~now o
      | Resume_order -> cmd_resume db ~config ?http ~now o
      | Cancel_order -> cmd_cancel db ~now o)

(* ── Entry point ────────────────────────────────────────────────────────── *)

let not_found id = Printf.sprintf "No standing order with id %s." id

let run ~db ~config ?http ~now request =
  (* Lazy expiry belongs to mutations. Inspection must remain read-only. *)
  (try
     match request with
     | Mutate _ -> ignore (Store.expire_pending db ~now : int)
     | _ -> ()
   with exn ->
     Logs.warn (fun m ->
         m "standing-order CLI: expire_pending failed: %s"
           (Printexc.to_string exn)));
  let with_order id f =
    match Store.find db id with
    | None -> (
        not_found id
        ^
        match request with
        | Show _ | Fires _ ->
            " Recovery readiness: "
            ^ R.readiness_name (R.rollout_readiness db ~order_id:id)
        | _ -> "")
    | Some o -> f o
  in
  match request with
  | List channel_id -> cmd_list db channel_id
  | Show id -> with_order id (cmd_show db)
  | Fires { id; limit } -> with_order id (cmd_fires db ~limit)
  | Mutate (mutation, id) ->
      with_order id (cmd_mutate db ~config ?http ~now mutation)

let cmd ~db ~config ?http ?now args =
  let now = match now with Some t -> t | None -> Store.now () in
  match args with
  | "orders" :: rest -> (
      match parse rest with
      | Error text -> text
      | Ok request -> (
          try run ~db ~config ?http ~now request
          with exn ->
            Printf.sprintf "Error: the standing-order command failed: %s"
              (Printexc.to_string exn)))
  | _ -> usage
