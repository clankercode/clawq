(** Standing-order persistence (spec §6.1): orders, the fire write-ahead ledger
    and bound cards.

    Every public function runs [ensure_schema] first. REAL columns hold unix
    seconds from the one standing-order clock ([clock]); TEXT audit columns are
    wall-clock [Time_util.sql_datetime_utc]. Rows whose [state] or [action_json]
    don't decode are skipped by every reader (fail-closed, logged by id only). A
    read that fails raises (see [Slack_standing_order_sql.query]); it never
    reads as no rows. *)

module T = Slack_standing_order_types
include Slack_standing_order_sql

let clock : (unit -> float) ref = ref Unix.gettimeofday
let now () = !clock ()
let default_max_fires_per_day = 10
let pending_ttl_seconds = 600.
let new_id ~prefix = prefix ^ "_" ^ Temp_downloads.generate_token ()
let ts_of_unix t = Printf.sprintf "%.6f" t

(* Values come from closed enumerations, never from input. *)
let sql_in values =
  String.concat ", " (List.map (fun v -> "'" ^ v ^ "'") values)

let fire_states_sql states = sql_in (List.map T.fire_state_to_string states)

let live_states_sql =
  sql_in (List.map T.state_to_string T.[ Pending_confirmation; Active; Paused ])

let schema =
  [
    "CREATE TABLE IF NOT EXISTS slack_standing_orders (\n\
    \  id TEXT PRIMARY KEY,\n\
    \  display_no INTEGER NOT NULL CHECK (display_no >= 1),\n\
    \  team_id TEXT NOT NULL, enterprise_id TEXT, api_app_id TEXT NOT NULL,\n\
    \  channel_id TEXT NOT NULL, grantor_user_id TEXT NOT NULL, principal_id \
     TEXT,\n\
    \  trigger_kind TEXT NOT NULL CHECK (trigger_kind IN \
     ('huddle_notes_ready')),\n\
    \  action_json TEXT NOT NULL, target_app_id TEXT NOT NULL,\n\
    \  state TEXT NOT NULL CHECK (state IN \
     ('pending_confirmation','active','paused','suspended','cancelled')),\n\
    \  active_since TEXT, epoch INTEGER NOT NULL DEFAULT 0, revision INTEGER \
     NOT NULL DEFAULT 1,\n\
    \  management_channel_id TEXT NOT NULL, management_thread_ts TEXT NOT NULL,\n\
    \  max_fires_per_day INTEGER NOT NULL DEFAULT 10, pending_expires_at REAL,\n\
    \  created_at TEXT NOT NULL, confirmed_at TEXT, confirmed_by TEXT, \
     expires_at TEXT,\n\
    \  paused_by TEXT, suspended_reason TEXT, cancelled_by TEXT, updated_at \
     TEXT NOT NULL,\n\
    \  bot_user_id TEXT, bot_id TEXT,\n\
    \  spoken_requests INTEGER NOT NULL DEFAULT 1 CHECK (spoken_requests IN \
     (0, 1)),\n\
    \  card_version INTEGER NOT NULL DEFAULT 1,\n\
    \  UNIQUE (team_id, channel_id, display_no))";
    "CREATE UNIQUE INDEX IF NOT EXISTS slack_standing_orders_one_live ON \
     slack_standing_orders(team_id, channel_id, trigger_kind, target_app_id) \
     WHERE state IN ('pending_confirmation','active','paused')";
    Printf.sprintf
      "CREATE TABLE IF NOT EXISTS slack_standing_order_fires (\n\
      \  id TEXT PRIMARY KEY, order_id TEXT NOT NULL, room_id TEXT NOT NULL,\n\
      \  date_start TEXT NOT NULL CHECK (date_start <> ''),\n\
      \  root_ts TEXT, canvas_file_id TEXT, transcript_file_id TEXT, \
       notes_ready_ts TEXT,\n\
      \  state TEXT NOT NULL CHECK (state IN (%s)), epoch INTEGER NOT NULL,\n\
      \  claim_owner TEXT, lease_until REAL, lease_epoch INTEGER NOT NULL \
       DEFAULT 0,\n\
      \  cancel_requested INTEGER NOT NULL DEFAULT 0, repost_forbidden INTEGER \
       NOT NULL DEFAULT 0,\n\
      \  nonce TEXT, uploaded_file_ids TEXT NOT NULL DEFAULT '[]', relay_ts \
       TEXT,\n\
      \  attempts INTEGER NOT NULL DEFAULT 0,\n\
      \  reposts INTEGER NOT NULL DEFAULT 0 CHECK (reposts BETWEEN 0 AND 1),\n\
      \  next_attempt_at REAL, dispatched_at REAL, reason TEXT,\n\
      \  source_canvas_id TEXT, source_transcript_id TEXT, \
       retention_checked_at TEXT,\n\
      \  created_at TEXT NOT NULL, updated_at TEXT NOT NULL,\n\
      \  UNIQUE (order_id, room_id, date_start), UNIQUE (order_id, \
       canvas_file_id))"
      (fire_states_sql T.all_fire_states);
    "CREATE UNIQUE INDEX IF NOT EXISTS slack_standing_order_fires_transcript \
     ON slack_standing_order_fires(transcript_file_id) WHERE \
     transcript_file_id IS NOT NULL";
    "CREATE TABLE IF NOT EXISTS slack_standing_order_cards (\n\
    \  id TEXT PRIMARY KEY,\n\
    \  kind TEXT NOT NULL CHECK (kind IN ('confirm','resume','pause','change')),\n\
    \  order_id TEXT NOT NULL, team_id TEXT NOT NULL, app_id TEXT NOT NULL, \
     channel_id TEXT NOT NULL,\n\
    \  reply_thread_ts TEXT, message_ts TEXT, plan_id TEXT, plan_digest TEXT,\n\
    \  order_epoch INTEGER NOT NULL, expires_at REAL, used_at TEXT,\n\
    \  created_by TEXT NOT NULL, created_at TEXT NOT NULL)";
  ]

let ensure_schema db = List.iter (Sql_util.exec_schema ~label db) schema

(* ── Order codec ────────────────────────────────────────────────────────── *)

let order_columns =
  columns
    "id display_no team_id enterprise_id api_app_id channel_id grantor_user_id \
     principal_id trigger_kind action_json target_app_id state active_since \
     epoch revision management_channel_id management_thread_ts \
     max_fires_per_day pending_expires_at created_at confirmed_at confirmed_by \
     expires_at paused_by suspended_reason cancelled_by updated_at bot_user_id \
     bot_id spoken_requests card_version"

let select_orders =
  "SELECT " ^ String.concat ", " order_columns ^ " FROM slack_standing_orders"

let target_app_id (o : T.order) = (T.action_target o.action).app_id

(* In [order_columns] order. *)
let order_params (o : T.order) =
  let open Sqlite3.Data in
  [
    text o.id;
    int o.display_no;
    text o.team_id;
    opt_text o.enterprise_id;
    text o.api_app_id;
    text o.channel_id;
    text o.grantor_user_id;
    opt_text o.principal_id;
    text (T.trigger_to_string o.trigger);
    text (Yojson.Safe.to_string (T.action_to_json o.action));
    text (target_app_id o);
    text (T.state_to_string o.state);
    opt_text o.active_since;
    int o.epoch;
    int o.revision;
    text o.management_channel_id;
    text o.management_thread_ts;
    int o.max_fires_per_day;
    opt_float o.pending_expires_at;
    text o.created_at;
    opt_text o.confirmed_at;
    opt_text o.confirmed_by;
    opt_text o.expires_at;
    opt_text o.paused_by;
    opt_text o.suspended_reason;
    opt_text o.cancelled_by;
    text o.updated_at;
    opt_text o.bot_user_id;
    opt_text o.bot_id;
    bool o.spoken_requests;
    int o.card_version;
  ]

let decode_order stmt : (T.order, string) result =
  let ( let* ) = Result.bind in
  let text i = Sql_util.text_column stmt i in
  let opt i = Sql_util.opt_text_column stmt i in
  let int i = Sql_util.int_column stmt i in
  let* trigger =
    Option.to_result ~none:"unknown trigger_kind" (T.trigger_of_string (text 8))
  in
  let* action =
    match Yojson.Safe.from_string (text 9) with
    | json -> T.action_of_json json
    | exception Yojson.Json_error _ -> Error "action_json is not JSON"
  in
  let* () =
    if (T.action_target action).app_id = text 10 then Ok ()
    else Error "target_app_id disagrees with action_json"
  in
  let* state =
    Option.to_result ~none:"unknown state" (T.state_of_string (text 11))
  in
  Ok
    ({
       id = text 0;
       display_no = int 1;
       team_id = text 2;
       enterprise_id = opt 3;
       api_app_id = text 4;
       channel_id = text 5;
       grantor_user_id = text 6;
       principal_id = opt 7;
       trigger;
       action;
       state;
       active_since = opt 12;
       epoch = int 13;
       revision = int 14;
       management_channel_id = text 15;
       management_thread_ts = text 16;
       max_fires_per_day = int 17;
       pending_expires_at = opt_float_column stmt 18;
       created_at = text 19;
       confirmed_at = opt 20;
       confirmed_by = opt 21;
       expires_at = opt 22;
       paused_by = opt 23;
       suspended_reason = opt 24;
       cancelled_by = opt 25;
       updated_at = text 26;
       bot_user_id = opt 27;
       bot_id = opt 28;
       spoken_requests = Sql_util.sql_bool stmt 29;
       card_version = int 30;
     }
      : T.order)

let skip_undecodable what id reason =
  Logs.warn (fun m ->
      m "%s: skipping undecodable %s %s: %s" label what id reason)

let query_orders db where params =
  query db
    (select_orders ^ " " ^ where)
    params
    (fun stmt -> (Sql_util.text_column stmt 0, decode_order stmt))
  |> List.filter_map (function
    | _, Ok o -> Some o
    | id, Error reason ->
        skip_undecodable "order" id reason;
        None)

let insert_order db (o : T.order) =
  step db
    (Printf.sprintf "INSERT INTO slack_standing_orders (%s) VALUES (%s)"
       (String.concat ", " order_columns)
       (String.concat ", " (List.map (fun _ -> "?") order_columns)))
    (order_params o)

(* The single writer for state changes: writes [after]'s mutable columns iff
   the row still has [before]'s state and epoch. Identity, grantor, target and
   action columns are never rewritten. *)
let update_order db ~(before : T.order) (after : T.order) =
  let open Sqlite3.Data in
  match
    step db
      "UPDATE slack_standing_orders SET state = ?, active_since = ?, epoch = \
       ?, revision = ?, pending_expires_at = ?, confirmed_at = ?, confirmed_by \
       = ?, paused_by = ?, suspended_reason = ?, cancelled_by = ?, updated_at \
       = ?, bot_user_id = ?, bot_id = ? WHERE id = ? AND state = ? AND epoch = \
       ?"
      [
        text (T.state_to_string after.state);
        opt_text after.active_since;
        int after.epoch;
        int after.revision;
        opt_float after.pending_expires_at;
        opt_text after.confirmed_at;
        opt_text after.confirmed_by;
        opt_text after.paused_by;
        opt_text after.suspended_reason;
        opt_text after.cancelled_by;
        text after.updated_at;
        opt_text after.bot_user_id;
        opt_text after.bot_id;
        text before.id;
        text (T.state_to_string before.state);
        int before.epoch;
      ]
  with
  | Sqlite3.Rc.DONE -> if Sqlite3.changes db = 1 then `Updated else `Stale
  | Sqlite3.Rc.CONSTRAINT -> `Conflict
  | rc ->
      failwith
        (Printf.sprintf "%s: order update failed: %s" label
           (Sqlite3.Rc.to_string rc))

(* ── Order reads ────────────────────────────────────────────────────────── *)

let find_raw db id = first (query_orders db "WHERE id = ?" [ text id ])

let find db id =
  ensure_schema db;
  find_raw db id

let find_by_display db ~team_id ~channel_id display_no =
  ensure_schema db;
  first
    (query_orders db "WHERE team_id = ? AND channel_id = ? AND display_no = ?"
       [ text team_id; text channel_id; int display_no ])

let list db ?team_id ?channel_id ?(include_cancelled = false) () =
  ensure_schema db;
  let filters =
    List.filter_map Fun.id
      [
        Option.map (fun t -> ("team_id = ?", [ text t ])) team_id;
        Option.map (fun c -> ("channel_id = ?", [ text c ])) channel_id;
        (if include_cancelled then None else Some ("state <> 'cancelled'", []));
      ]
  in
  let where =
    match filters with
    | [] -> ""
    | _ -> "WHERE " ^ String.concat " AND " (List.map fst filters)
  in
  query_orders db
    (where ^ " ORDER BY team_id, channel_id, display_no")
    (List.concat_map snd filters)

(* Any row in any state, undecodable rows included: whether a Plan evaluation
   ever admitted this channel (Decision 20). *)
let channel_has_orders db ~team_id ~api_app_id ~channel_id =
  ensure_schema db;
  int_query db
    "SELECT EXISTS (SELECT 1 FROM slack_standing_orders WHERE team_id = ? AND \
     api_app_id = ? AND channel_id = ?)"
    [ text team_id; text api_app_id; text channel_id ]
  = 1

(* The orders that make a channel observed (§6.2). The huddle observer asks
   per inbound event, so this is one SELECT on the (team_id, channel_id)
   prefix of the display-number key and never a Slack call. *)
let observed_orders db ~team_id ~api_app_id ~channel_id =
  ensure_schema db;
  query_orders db
    ("WHERE team_id = ? AND channel_id = ? AND api_app_id = ? AND state IN ("
    ^ sql_in (List.map T.state_to_string T.[ Active; Paused ])
    ^ ") ORDER BY display_no")
    [ text team_id; text channel_id; text api_app_id ]

let live_where =
  "WHERE team_id = ? AND channel_id = ? AND target_app_id = ? AND id <> ? AND \
   state IN (" ^ live_states_sql ^ ") ORDER BY display_no"

let live_params ~team_id ~channel_id ~target_app_id ~excluding =
  [ text team_id; text channel_id; text target_app_id; text excluding ]

let find_live_conflict db ~team_id ~channel_id ~target_app_id ~excluding =
  ensure_schema db;
  first
    (query_orders db live_where
       (live_params ~team_id ~channel_id ~target_app_id ~excluding))

(* Raw, so a live row that no longer decodes still names its SO number. *)
let live_display_no db ~team_id ~channel_id ~target_app_id ~excluding =
  first
    (query db ("SELECT display_no FROM slack_standing_orders " ^ live_where)
       (live_params ~team_id ~channel_id ~target_app_id ~excluding) (fun stmt ->
         Sql_util.int_column stmt 0))

(* ── Create and activate ────────────────────────────────────────────────── *)

type insert_error =
  | Duplicate_live of { display_no : int }
  | Insert_failed of string

let next_display_no db ~team_id ~channel_id =
  int_query db
    "SELECT COALESCE(MAX(display_no), 0) + 1 FROM slack_standing_orders WHERE \
     team_id = ? AND channel_id = ?"
    [ text team_id; text channel_id ]

let insert_pending db ~id ~team_id ?enterprise_id ~api_app_id ~channel_id
    ~grantor_user_id ?principal_id ~action ~management_thread_ts ~now () =
  ensure_schema db;
  in_tx db
    ~on_error:(fun e -> Insert_failed e)
    (fun () ->
      let created = stamp () in
      let order : T.order =
        {
          id;
          display_no = next_display_no db ~team_id ~channel_id;
          team_id;
          enterprise_id;
          api_app_id;
          channel_id;
          grantor_user_id;
          principal_id;
          trigger = T.Huddle_notes_ready;
          action;
          state = T.Pending_confirmation;
          active_since = None;
          epoch = 0;
          revision = 1;
          management_channel_id = channel_id;
          management_thread_ts;
          max_fires_per_day = default_max_fires_per_day;
          pending_expires_at = Some (now +. pending_ttl_seconds);
          created_at = created;
          confirmed_at = None;
          confirmed_by = None;
          expires_at = None;
          paused_by = None;
          suspended_reason = None;
          cancelled_by = None;
          updated_at = created;
          bot_user_id = None;
          bot_id = None;
          spoken_requests = true;
          card_version = T.current_card_version;
        }
      in
      match insert_order db order with
      | Sqlite3.Rc.DONE -> (
          (* An order that can't be read back would hold the live slot
             while every reader skips it. *)
          match find_raw db id with
          | Some o -> Ok o
          | None -> Error (Insert_failed "the new order does not read back"))
      | Sqlite3.Rc.CONSTRAINT -> (
          match
            live_display_no db ~team_id ~channel_id
              ~target_app_id:(target_app_id order) ~excluding:id
          with
          | Some display_no -> Error (Duplicate_live { display_no })
          | None -> Error (Insert_failed "constraint violation"))
      | rc -> Error (Insert_failed (Sqlite3.Rc.to_string rc)))

(* Installation facts written at confirm and resume (Decision 16). *)
let check_install ~active_since ~bot_user_id ~bot_id =
  if not (T.valid_slack_ts active_since) then
    Error "active_since is not a Slack timestamp"
  else if not (Slack_config_types.valid_id "UW" bot_user_id) then
    Error "bot_user_id is not a Slack user id"
  else if not (Slack_config_types.valid_id "B" bot_id) then
    Error "bot_id is not a Slack bot id"
  else Ok ()

let pending_open (o : T.order) ~now =
  match o.pending_expires_at with Some t -> t > now | None -> false

let activate_in_tx db ~order_id ~active_since ~confirmed_by ~bot_user_id ~bot_id
    ~now =
  guard (fun () ->
      ensure_schema db;
      let ( let* ) = Result.bind in
      let* () = check_install ~active_since ~bot_user_id ~bot_id in
      let* (before : T.order) =
        Option.to_result ~none:"standing order not found" (find_raw db order_id)
      in
      let* () =
        if before.state <> T.Pending_confirmation then
          Error "standing order is not pending confirmation"
        else if not (pending_open before ~now) then
          Error "the confirmation window has expired"
        else Ok ()
      in
      let at = stamp () in
      let after =
        {
          before with
          state = T.Active;
          active_since = Some active_since;
          epoch = before.epoch + 1;
          pending_expires_at = None;
          confirmed_at = Some at;
          confirmed_by = Some confirmed_by;
          updated_at = at;
          bot_user_id = Some bot_user_id;
          bot_id = Some bot_id;
        }
      in
      match update_order db ~before after with
      | `Updated -> Ok after
      | `Stale -> Error "standing order changed concurrently"
      | `Conflict -> Error "standing order conflicts with a live order")

(* ── Deactivate (pause, suspend, cancel, withdraw) ──────────────────────── *)

type deactivation =
  | Pause of string
  | Suspend of string
  | Cancel of string
  | Withdraw of string

type state_change = {
  before : T.order;
  after : T.order;
  changed : bool;
  in_flight : int option;
      (** Fires in [posting] or [posting_unknown]; [None] if the count failed.
      *)
}

let transition (o : T.order) = function
  | Pause by when o.state = T.Active ->
      Some { o with state = T.Paused; paused_by = Some by }
  | Suspend reason when o.state = T.Active ->
      Some { o with state = T.Suspended; suspended_reason = Some reason }
  | Cancel by when o.state <> T.Cancelled ->
      Some { o with state = T.Cancelled; cancelled_by = Some by }
  | Withdraw by when o.state = T.Pending_confirmation ->
      Some { o with state = T.Cancelled; cancelled_by = Some by }
  | Pause _ | Suspend _ | Cancel _ | Withdraw _ -> None

let no_live_claim =
  "(claim_owner IS NULL OR lease_until IS NULL OR lease_until < ?)"

(* Pre-dispatch fires stop (or are asked to, under a live claim); dispatched
   ones keep their state but may never be re-posted. *)
let sweep_fires db ~order_id ~now =
  let at = stamp () in
  let pre = fire_states_sql T.pre_dispatch_states in
  exec db
    (Printf.sprintf
       "UPDATE slack_standing_order_fires SET state = 'skipped_inactive', \
        reason = 'order_inactive', updated_at = ? WHERE order_id = ? AND state \
        IN (%s) AND %s"
       pre no_live_claim)
    [ text at; text order_id; Sqlite3.Data.FLOAT now ];
  exec db
    (Printf.sprintf
       "UPDATE slack_standing_order_fires SET cancel_requested = 1, updated_at \
        = ? WHERE order_id = ? AND state IN (%s) AND NOT %s"
       pre no_live_claim)
    [ text at; text order_id; Sqlite3.Data.FLOAT now ];
  exec db
    (Printf.sprintf
       "UPDATE slack_standing_order_fires SET repost_forbidden = 1, updated_at \
        = ? WHERE order_id = ? AND state IN (%s)"
       (fire_states_sql T.dispatched_states))
    [ text at; text order_id ];
  (* The receipt count is diagnostic; only this read may fail without
     rolling back the order change and the completed sweep writes. *)
  try
    Some
      (int_query db
         (Printf.sprintf
            "SELECT COUNT(*) FROM slack_standing_order_fires WHERE order_id = \
             ? AND state IN (%s)"
            (fire_states_sql T.[ Posting; Posting_unknown ]))
         [ text order_id ])
  with exn when is_db_error exn ->
    Logs.warn (fun m ->
        m "%s: in-flight count failed for %s: %s" label order_id
          (Printexc.to_string exn));
    None

let deactivate_in_tx db ~order_id op ~now =
  guard (fun () ->
      ensure_schema db;
      match find_raw db order_id with
      | None -> Error "standing order not found"
      | Some before -> (
          match transition before op with
          | None ->
              Ok { before; after = before; changed = false; in_flight = Some 0 }
          | Some next -> (
              let after =
                {
                  next with
                  epoch = before.epoch + 1;
                  revision = before.revision + 1;
                  updated_at = stamp ();
                }
              in
              match update_order db ~before after with
              | `Updated ->
                  let in_flight = sweep_fires db ~order_id ~now in
                  Ok { before; after; changed = true; in_flight }
              | `Stale -> Error "standing order changed concurrently"
              | `Conflict -> Error "standing order conflicts with a live order")
          ))

let deactivate db ~order_id op ~now =
  ensure_schema db;
  in_tx db ~on_error:Fun.id (fun () -> deactivate_in_tx db ~order_id op ~now)

let expired_where =
  "WHERE state = 'pending_confirmation' AND (pending_expires_at IS NULL OR \
   pending_expires_at <= ?) ORDER BY id"

(* Callers run this before plans, lookups and clicks; it never raises, so a
   failure here (logged) can't block a pause. *)
let expire_pending db ~now =
  let failed reason =
    Logs.warn (fun m -> m "%s: expire_pending failed: %s" label reason);
    0
  in
  let expired () = query_orders db expired_where [ Sqlite3.Data.FLOAT now ] in
  match
    ensure_schema db;
    expired ()
  with
  | exception exn when is_db_error exn -> failed (Printexc.to_string exn)
  | [] -> 0
  | _ -> (
      let withdraw count (o : T.order) =
        Result.bind count (fun n ->
            Result.map
              (fun c -> if c.changed then n + 1 else n)
              (deactivate_in_tx db ~order_id:o.id (Withdraw "expired") ~now))
      in
      match
        in_tx db ~on_error:Fun.id (fun () ->
            List.fold_left withdraw (Ok 0) (expired ()))
      with
      | Ok n -> n
      | Error reason -> failed reason)

type resume_error =
  | Not_resumable of T.state
  | Stale_epoch
  | Duplicate_live_on_resume of int
  | Resume_failed of string

let resume db ~order_id ~expected_epoch ~active_since ~bot_user_id ~bot_id
    ~now:_ =
  ensure_schema db;
  match check_install ~active_since ~bot_user_id ~bot_id with
  | Error e -> Error (Resume_failed e)
  | Ok () ->
      in_tx db
        ~on_error:(fun e -> Resume_failed e)
        (fun () ->
          match find_raw db order_id with
          | None -> Error (Resume_failed "standing order not found")
          | Some before
            when not (before.state = T.Paused || before.state = T.Suspended) ->
              Error (Not_resumable before.state)
          | Some before when before.epoch <> expected_epoch -> Error Stale_epoch
          | Some before -> (
              let after =
                {
                  before with
                  state = T.Active;
                  active_since = Some active_since;
                  epoch = before.epoch + 1;
                  revision = before.revision + 1;
                  paused_by = None;
                  suspended_reason = None;
                  updated_at = stamp ();
                  bot_user_id = Some bot_user_id;
                  bot_id = Some bot_id;
                }
              in
              match update_order db ~before after with
              | `Updated -> Ok after
              | `Stale -> Error Stale_epoch
              | `Conflict -> (
                  match
                    live_display_no db ~team_id:before.team_id
                      ~channel_id:before.channel_id
                      ~target_app_id:(target_app_id before) ~excluding:order_id
                  with
                  | Some n -> Error (Duplicate_live_on_resume n)
                  | None -> Error (Resume_failed "constraint violation"))))
