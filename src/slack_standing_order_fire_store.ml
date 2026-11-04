(** Standing-order fire reads and inserts. Lifecycle sweeps remain in Base. *)

open Slack_standing_order_store_base

let opt_text = Sqlite3.Data.opt_text
let opt_float = Sqlite3.Data.opt_float

(* ── Fires ──────────────────────────────────────────────────────────────── *)

let fire_columns =
  columns
    "id order_id room_id date_start state epoch relay_ts reason \
     repost_forbidden cancel_requested reposts dispatched_at created_at \
     updated_at"

let select_fires =
  "SELECT "
  ^ String.concat ", " fire_columns
  ^ " FROM slack_standing_order_fires"

let decode_fire stmt : (T.fire, string) result =
  let text i = Sql_util.text_column stmt i in
  let opt i = Sql_util.opt_text_column stmt i in
  match T.fire_state_of_string (text 4) with
  | None -> Error "unknown fire state"
  | Some state ->
      Ok
        ({
           id = text 0;
           order_id = text 1;
           room_id = text 2;
           date_start = text 3;
           state;
           epoch = Sql_util.int_column stmt 5;
           relay_ts = opt 6;
           reason = opt 7;
           repost_forbidden = Sql_util.sql_bool stmt 8;
           cancel_requested = Sql_util.sql_bool stmt 9;
           reposts = Sql_util.int_column stmt 10;
           dispatched_at = opt_float_column stmt 11;
           created_at = text 12;
           updated_at = text 13;
         }
          : T.fire)

let query_fires db where params =
  query db
    (select_fires ^ " " ^ where)
    params
    (fun stmt -> (Sql_util.text_column stmt 0, decode_fire stmt))
  |> List.filter_map (function
    | _, Ok f -> Some f
    | id, Error reason ->
        skip_undecodable "fire" id reason;
        None)

let insert_fire db ~(order : T.order) ~room_id ~date_start ?canvas_file_id
    ?transcript_file_id ?notes_ready_ts ~now:_ () =
  if date_start = "" then Error "date_start is empty"
  else begin
    ensure_schema db;
    in_tx db ~on_error:Fun.id (fun () ->
        let exists =
          int_query db
            "SELECT COUNT(*) FROM slack_standing_order_fires WHERE order_id = \
             ? AND room_id = ? AND date_start = ?"
            [ text order.id; text room_id; text date_start ]
          > 0
        in
        if exists then Ok None
        else
          let id = new_id ~prefix:"sof" in
          let at = stamp () in
          let open Sqlite3.Data in
          (* Never INSERT OR IGNORE: that would swallow the canvas and
             transcript uniqueness violations. *)
          match
            step db
              "INSERT INTO slack_standing_order_fires (id, order_id, room_id, \
               date_start, canvas_file_id, transcript_file_id, notes_ready_ts, \
               state, epoch, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, \
               ?, ?, ?, ?, ?)"
              [
                text id;
                text order.id;
                text room_id;
                text date_start;
                opt_text canvas_file_id;
                opt_text transcript_file_id;
                opt_text notes_ready_ts;
                text (T.fire_state_to_string T.Detected);
                int order.epoch;
                text at;
                text at;
              ]
          with
          | Sqlite3.Rc.DONE -> (
              match query_fires db "WHERE id = ?" [ text id ] with
              | f :: _ -> Ok (Some f)
              | [] -> Error "the new fire does not read back")
          | Sqlite3.Rc.CONSTRAINT -> Error "fire_conflict"
          | rc -> Error (Sqlite3.Rc.to_string rc))
  end

let list_fires db ~order_id ~limit =
  ensure_schema db;
  query_fires db
    "WHERE order_id = ? ORDER BY created_at DESC, rowid DESC LIMIT ?"
    [ text order_id; int (max 0 limit) ]

let dispatches_since db ~order_id ~since =
  ensure_schema db;
  (* Corrupt legacy counters must fail closed even after an earlier import. *)
  if
    int_query db
      "SELECT COUNT(*) FROM slack_standing_order_fires WHERE order_id = ? AND \
       reposts NOT BETWEEN 0 AND 1"
      [ text order_id ]
    <> 0
  then failwith "invalid legacy re-post count";
  int_query db
    "SELECT COUNT(*) FROM slack_standing_order_dispatches d JOIN \
     slack_standing_order_fires f ON f.id=d.fire_id WHERE f.order_id = ? AND \
     d.dispatched_at IS NOT NULL AND d.dispatched_at >= ?"
    [ text order_id; Sqlite3.Data.FLOAT since ]

type lease = { owner : string; lease_epoch : int; until : float }

type relay_attempt = {
  attempt_no : int;
  uploaded_file_ids : string list;
  relay_ts : string option;
  upload_started_at : float option;
  dispatched_at : float option;
  pickup_started_at : float option;
  first_reply_ts : string option;
  rate_limited_at : float option;
  reaction_seen : bool;
  acknowledged_at : float option;
  picked_up_at : float option;
  not_delivered : bool;
}

type fire_runtime = {
  fire : T.fire;
  root_ts : string option;
  canvas_id : string option;
  transcript_id : string option;
  notes_ready_ts : string option;
  nonce : string option;
  attempts : relay_attempt list;
  current_attempt : int;
  next_attempt_at : float option;
  claim : lease option;
}

let lease_seconds = 120.
let renewal_seconds = 30.

let empty_attempt attempt_no : relay_attempt =
  {
    attempt_no;
    uploaded_file_ids = [];
    relay_ts = None;
    upload_started_at = None;
    dispatched_at = None;
    pickup_started_at = None;
    first_reply_ts = None;
    rate_limited_at = None;
    reaction_seen = false;
    acknowledged_at = None;
    picked_up_at = None;
    not_delivered = false;
  }

let attempt_columns =
  "attempt_no,uploaded_file_ids,relay_ts,upload_started_at,dispatched_at,pickup_started_at,first_reply_ts,rate_limited_at,reaction_seen,acknowledged_at,picked_up_at,not_delivered"

let decode_attempt stmt : relay_attempt =
  let ids =
    match Yojson.Safe.from_string (Sql_util.text_column stmt 1) with
    | `List xs ->
        List.map
          (function `String s -> s | _ -> failwith "invalid uploaded file id")
          xs
    | _ -> failwith "invalid uploaded file ids"
    | exception Yojson.Json_error _ -> failwith "invalid uploaded file ids"
  in
  {
    attempt_no = Sql_util.int_column stmt 0;
    uploaded_file_ids = ids;
    relay_ts = Sql_util.opt_text_column stmt 2;
    upload_started_at = opt_float_column stmt 3;
    dispatched_at = opt_float_column stmt 4;
    pickup_started_at = opt_float_column stmt 5;
    first_reply_ts = Sql_util.opt_text_column stmt 6;
    rate_limited_at = opt_float_column stmt 7;
    reaction_seen = Sql_util.sql_bool stmt 8;
    acknowledged_at = opt_float_column stmt 9;
    picked_up_at = opt_float_column stmt 10;
    not_delivered = Sql_util.sql_bool stmt 11;
  }

let runtime_raw db ~fire_id =
  match query_fires db "WHERE id = ?" [ text fire_id ] with
  | [] -> None
  | fire :: _ ->
      let attempts =
        query db
          ("SELECT " ^ attempt_columns
         ^ " FROM slack_standing_order_dispatches WHERE fire_id=? ORDER BY \
            attempt_no")
          [ text fire_id ]
          decode_attempt
      in
      query db
        "SELECT \
         root_ts,canvas_file_id,transcript_file_id,notes_ready_ts,nonce,current_attempt,next_attempt_at,claim_owner,lease_epoch,lease_until \
         FROM slack_standing_order_fires WHERE id=?"
        [ text fire_id ]
        (fun s ->
          let opt = Sql_util.opt_text_column s in
          let claim =
            match (opt 7, opt_float_column s 9) with
            | Some owner, Some until ->
                Some { owner; until; lease_epoch = Sql_util.int_column s 8 }
            | _ -> None
          in
          {
            fire;
            root_ts = opt 0;
            canvas_id = opt 1;
            transcript_id = opt 2;
            notes_ready_ts = opt 3;
            nonce = opt 4;
            attempts;
            current_attempt = Sql_util.int_column s 5;
            next_attempt_at = opt_float_column s 6;
            claim;
          })
      |> first

let runtime db ~fire_id =
  ensure_schema db;
  runtime_raw db ~fire_id

let due db ~now =
  ensure_schema db;
  (* Retained relays remain observable for 30 days, including late notices
     after pickup. A scheduled time takes precedence over eager polling. *)
  query db
    "SELECT f.id FROM slack_standing_order_fires f WHERE (f.next_attempt_at IS \
     NULL OR f.next_attempt_at <= ?) AND (f.claim_owner IS NULL OR \
     f.lease_until IS NULL OR f.lease_until <= ?) AND (f.state IN \
     ('detected','fetching','uploading','posting','posting_unknown') OR EXISTS \
     (SELECT 1 FROM slack_standing_order_dispatches d WHERE d.fire_id=f.id AND \
     d.dispatched_at >= ?)) ORDER BY f.rowid"
    [
      Sqlite3.Data.FLOAT now;
      Sqlite3.Data.FLOAT now;
      Sqlite3.Data.FLOAT (now -. (30. *. 86400.));
    ]
    (fun s -> Sql_util.text_column s 0)
  |> List.filter_map (fun fire_id -> runtime_raw db ~fire_id)

let claim db ~fire_id ~owner ~now =
  guard (fun () ->
      ensure_schema db;
      if owner = "" then Error "lease owner is empty"
      else
        in_tx db ~on_error:Fun.id (fun () ->
            exec db
              "UPDATE slack_standing_order_fires SET \
               claim_owner=?,lease_epoch=lease_epoch+1,lease_until=? WHERE \
               id=? AND (claim_owner IS NULL OR lease_until IS NULL OR \
               lease_until <= ?) AND EXISTS (SELECT 1 FROM \
               slack_standing_orders WHERE id=order_id) AND NOT EXISTS (SELECT \
               1 FROM slack_standing_order_fires other JOIN \
               slack_standing_orders oo ON oo.id=other.order_id JOIN \
               slack_standing_orders own ON \
               own.id=slack_standing_order_fires.order_id WHERE \
               other.id<>slack_standing_order_fires.id AND \
               oo.team_id=own.team_id AND oo.api_app_id=own.api_app_id AND \
               oo.channel_id=own.channel_id AND other.claim_owner IS NOT NULL \
               AND other.lease_until > ?)"
              [
                text owner;
                Sqlite3.Data.FLOAT (now +. lease_seconds);
                text fire_id;
                Sqlite3.Data.FLOAT now;
                Sqlite3.Data.FLOAT now;
              ];
            if Sqlite3.changes db = 0 then Ok None
            else Ok (Option.bind (runtime_raw db ~fire_id) (fun r -> r.claim))))

let valid_lease (lease : lease) (r : fire_runtime) now =
  match r.claim with
  | Some c ->
      c.owner = lease.owner
      && c.lease_epoch = lease.lease_epoch
      && c.until > now && lease.until > now
  | None -> false

let renew db ~fire_id ~(lease : lease) ~now =
  guard (fun () ->
      ensure_schema db;
      in_tx db ~on_error:Fun.id (fun () ->
          exec db
            "UPDATE slack_standing_order_fires SET lease_until=? WHERE id=? \
             AND claim_owner=? AND lease_epoch=? AND lease_until > ? AND ? > ?"
            [
              Sqlite3.Data.FLOAT (now +. lease_seconds);
              text fire_id;
              text lease.owner;
              int lease.lease_epoch;
              Sqlite3.Data.FLOAT now;
              Sqlite3.Data.FLOAT lease.until;
              Sqlite3.Data.FLOAT now;
            ];
          Ok
            (if Sqlite3.changes db = 1 then
               Some { lease with until = now +. lease_seconds }
             else None)))

let release db ~fire_id ~(lease : lease) ~now =
  ensure_schema db;
  exec db
    "UPDATE slack_standing_order_fires SET claim_owner=NULL,lease_until=NULL \
     WHERE id=? AND claim_owner=? AND lease_epoch=? AND lease_until > ? AND ? \
     > ?"
    [
      text fire_id;
      text lease.owner;
      int lease.lease_epoch;
      Sqlite3.Data.FLOAT now;
      Sqlite3.Data.FLOAT lease.until;
      Sqlite3.Data.FLOAT now;
    ]

let relay_allowed db (r : fire_runtime) =
  match find_raw db r.fire.order_id with
  | Some o ->
      o.state = T.Active && o.epoch = r.fire.epoch
      && (not r.fire.cancel_requested)
      && activation_announced db ~order_id:o.id
  | None -> false

let attempt (r : fire_runtime) n =
  List.find_opt (fun (a : relay_attempt) -> a.attempt_no = n) r.attempts

let save_attempt db fire_id (a : relay_attempt) =
  exec db
    ("INSERT INTO slack_standing_order_dispatches (fire_id," ^ attempt_columns
   ^ ") VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(fire_id,attempt_no) DO \
      UPDATE SET \
      uploaded_file_ids=excluded.uploaded_file_ids,relay_ts=excluded.relay_ts,upload_started_at=excluded.upload_started_at,dispatched_at=excluded.dispatched_at,pickup_started_at=excluded.pickup_started_at,first_reply_ts=excluded.first_reply_ts,rate_limited_at=excluded.rate_limited_at,reaction_seen=excluded.reaction_seen,acknowledged_at=excluded.acknowledged_at,picked_up_at=excluded.picked_up_at,not_delivered=excluded.not_delivered"
    )
    [
      text fire_id;
      int a.attempt_no;
      text
        (Yojson.Safe.to_string
           (`List (List.map (fun s -> `String s) a.uploaded_file_ids)));
      opt_text a.relay_ts;
      opt_float a.upload_started_at;
      opt_float a.dispatched_at;
      opt_float a.pickup_started_at;
      opt_text a.first_reply_ts;
      opt_float a.rate_limited_at;
      bool a.reaction_seen;
      opt_float a.acknowledged_at;
      opt_float a.picked_up_at;
      bool a.not_delivered;
    ]

let declared_transition before after =
  before = after
  ||
  match (before, after) with
  | T.Detected, T.Fetching
  | T.Fetching, T.Uploading
  | (T.Posting | T.Posting_unknown), T.Posted
  | T.Posting, T.Posting_unknown
  | (T.Uploading | T.Fetching), T.Posted
  | (T.Posted | T.Posting | T.Posting_unknown | T.No_pickup), T.Acknowledged
  | ( (T.Posted | T.Posting | T.Posting_unknown | T.Acknowledged | T.No_pickup),
      T.Picked_up )
  | ( ( T.Posted | T.Acknowledged | T.Picked_up | T.No_pickup | T.Posting
      | T.Posting_unknown ),
      T.Not_delivered )
  | (T.Posted | T.Posting_unknown), T.No_pickup
  | ( (T.Detected | T.Fetching | T.Uploading),
      ( T.No_notes | T.Unavailable | T.Skipped_no_transcript | T.Denied
      | T.Skipped_inactive | T.Skipped_cap | T.Failed ) )
  | (T.Posting | T.Posting_unknown | T.Posted), (T.Failed | T.Skipped_cap) ->
      true
  | _ -> false

let immutable_fire (a : T.fire) (b : T.fire) =
  a.id = b.id && a.order_id = b.order_id && a.room_id = b.room_id
  && a.date_start = b.date_start
  && a.epoch = b.epoch
  && a.created_at = b.created_at
  && a.reposts = b.reposts
  && a.dispatched_at = b.dispatched_at

let kept old next = old = None || old = next

let normalize_attempt (a : relay_attempt) =
  match a.relay_ts with
  | None ->
      if a.pickup_started_at = None then Ok a
      else Error "pickup requires a relay timestamp"
  | Some ts when T.valid_slack_ts ts ->
      let seconds = float_of_string ts in
      if a.pickup_started_at = None || a.pickup_started_at = Some seconds then
        Ok { a with pickup_started_at = Some seconds }
      else Error "pickup must start at the Slack relay timestamp"
  | Some _ -> Error "invalid relay timestamp"

let validate_attempts (before : fire_runtime) (after : fire_runtime) =
  let ( let* ) = Result.bind in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest ->
        let* a = normalize_attempt a in
        let old =
          Option.value
            ~default:(empty_attempt a.attempt_no)
            (attempt before a.attempt_no)
        in
        let ids_kept =
          old.uploaded_file_ids = []
          || old.uploaded_file_ids = a.uploaded_file_ids
          || old.dispatched_at = None
             && List.for_all
                  (fun id -> List.mem id a.uploaded_file_ids)
                  old.uploaded_file_ids
        in
        if
          (not (List.mem a.attempt_no [ 0; 1 ]))
          || a.dispatched_at <> old.dispatched_at
          || (not ids_kept)
          || (not (kept old.relay_ts a.relay_ts))
          || (not (kept old.upload_started_at a.upload_started_at))
          || (not (kept old.first_reply_ts a.first_reply_ts))
          || (not (kept old.rate_limited_at a.rate_limited_at))
          || (not (kept old.acknowledged_at a.acknowledged_at))
          || (not (kept old.picked_up_at a.picked_up_at))
          || (old.reaction_seen && not a.reaction_seen)
          || (old.not_delivered && not a.not_delivered)
        then Error "attempt identity or evidence cannot be overwritten"
        else loop (a :: acc) rest
  in
  let numbers =
    List.map (fun (a : relay_attempt) -> a.attempt_no) after.attempts
  in
  if
    List.length numbers <> List.length (List.sort_uniq compare numbers)
    || List.exists
         (fun (a : relay_attempt) -> not (List.mem a.attempt_no numbers))
         before.attempts
  then Error "retained attempts cannot be removed or duplicated"
  else loop [] after.attempts

let relay_producing (before : fire_runtime) (after : fire_runtime) =
  before.fire.state <> after.fire.state
  && List.mem after.fire.state T.[ Fetching; Uploading ]
  || after.current_attempt <> before.current_attempt
  || List.exists
       (fun (a : relay_attempt) ->
         let old =
           Option.value
             ~default:(empty_attempt a.attempt_no)
             (attempt before a.attempt_no)
         in
         (* Resolving the file set of an already reserved, shared relay is
          read-only adoption. New upload preparation still needs authority. *)
         a.uploaded_file_ids <> old.uploaded_file_ids
         && not (old.dispatched_at <> None && a.relay_ts <> None)
         || a.upload_started_at <> old.upload_started_at)
       after.attempts

let cas db ~(lease : lease) ~(before : fire_runtime) ~(after : fire_runtime)
    ~now =
  guard (fun () ->
      ensure_schema db;
      in_tx db ~on_error:Fun.id (fun () ->
          match runtime_raw db ~fire_id:before.fire.id with
          | None -> Ok false
          | Some current
            when (not (valid_lease lease current now))
                 || current.fire.state <> before.fire.state ->
              Ok false
          | Some current ->
              let ( let* ) = Result.bind in
              if
                (not (immutable_fire before.fire after.fire))
                || (not
                      (declared_transition before.fire.state after.fire.state))
                || after.claim <> before.claim
                || after.current_attempt < before.current_attempt
                || (not (List.mem after.current_attempt [ 0; 1 ]))
                || (not (kept before.nonce after.nonce))
                || (not (kept before.root_ts after.root_ts))
                || (not (kept before.canvas_id after.canvas_id))
                || not (kept before.transcript_id after.transcript_id)
              then Error "invalid fire transition or immutable scope change"
              else if
                current.attempts <> before.attempts
                || current.current_attempt <> before.current_attempt
                || current.root_ts <> before.root_ts
                || current.canvas_id <> before.canvas_id
                || current.transcript_id <> before.transcript_id
                || current.notes_ready_ts <> before.notes_ready_ts
                || current.nonce <> before.nonce
                || current.next_attempt_at <> before.next_attempt_at
              then Ok false
              else if
                relay_producing before after
                && ((not (relay_allowed db current))
                   || current.fire.repost_forbidden)
              then Ok false
              else
                let* attempts = validate_attempts current after in
                let projected =
                  List.find_opt
                    (fun (a : relay_attempt) ->
                      a.attempt_no = after.current_attempt)
                    attempts
                in
                let relay_ts = Option.bind projected (fun a -> a.relay_ts) in
                if after.fire.relay_ts <> relay_ts then
                  Error "fire relay timestamp must project its current attempt"
                else
                  let ids =
                    match projected with
                    | None -> []
                    | Some a -> a.uploaded_file_ids
                  in
                  exec db
                    "UPDATE slack_standing_order_fires SET \
                     state=?,reason=?,root_ts=?,canvas_file_id=?,transcript_file_id=?,notes_ready_ts=?,nonce=?,current_attempt=?,next_attempt_at=?,relay_ts=?,uploaded_file_ids=?,repost_forbidden=?,cancel_requested=?,updated_at=? \
                     WHERE id=? AND state=? AND claim_owner=? AND \
                     lease_epoch=? AND lease_until > ?"
                    [
                      text (T.fire_state_to_string after.fire.state);
                      opt_text after.fire.reason;
                      opt_text after.root_ts;
                      opt_text after.canvas_id;
                      opt_text after.transcript_id;
                      opt_text after.notes_ready_ts;
                      opt_text after.nonce;
                      int after.current_attempt;
                      opt_float after.next_attempt_at;
                      opt_text relay_ts;
                      text
                        (Yojson.Safe.to_string
                           (`List (List.map (fun s -> `String s) ids)));
                      bool
                        (current.fire.repost_forbidden
                       || after.fire.repost_forbidden);
                      bool
                        (current.fire.cancel_requested
                       || after.fire.cancel_requested);
                      text (stamp ());
                      text before.fire.id;
                      text (T.fire_state_to_string before.fire.state);
                      text lease.owner;
                      int lease.lease_epoch;
                      Sqlite3.Data.FLOAT now;
                    ];
                  let changed = Sqlite3.changes db = 1 in
                  if changed then
                    List.iter (save_attempt db before.fire.id) attempts;
                  Ok changed))

let reserve_dispatch db ~fire_id ~(lease : lease) ~repost ~now =
  guard (fun () ->
      ensure_schema db;
      in_tx db ~on_error:Fun.id (fun () ->
          match runtime_raw db ~fire_id with
          | None -> Ok false
          | Some r
            when (not (valid_lease lease r now))
                 || (not (relay_allowed db r))
                 || r.fire.repost_forbidden ->
              Ok false
          | Some r -> (
              let no = if repost then 1 else 0 in
              let a = Option.value ~default:(empty_attempt no) (attempt r no) in
              let eligible =
                if repost then
                  r.fire.reposts = 0
                  && List.mem r.fire.state
                       T.[ Posting_unknown; Posted; Uploading ]
                  && List.exists
                       (fun (a : relay_attempt) ->
                         a.attempt_no = 0 && a.dispatched_at <> None)
                       r.attempts
                  && not
                       (List.exists
                          (fun (a : relay_attempt) ->
                            a.reaction_seen || a.acknowledged_at <> None
                            || a.picked_up_at <> None || a.not_delivered)
                          r.attempts)
                else
                  r.fire.reposts = 0 && r.current_attempt = 0
                  && r.fire.state = T.Uploading
              in
              if (not eligible) || a.dispatched_at <> None || a.relay_ts <> None
              then Ok false
              else
                match find_raw db r.fire.order_id with
                | None -> Ok false
                | Some o ->
                    let count =
                      dispatches_since db ~order_id:o.id ~since:(now -. 86400.)
                    in
                    if count >= o.max_fires_per_day then Ok false
                    else
                      let a = { a with dispatched_at = Some now } in
                      save_attempt db fire_id a;
                      exec db
                        "UPDATE slack_standing_order_fires SET \
                         state='posting',dispatched_at=COALESCE(dispatched_at,?),reposts=?,current_attempt=?,relay_ts=?,uploaded_file_ids=?,next_attempt_at=NULL,updated_at=? \
                         WHERE id=? AND state=? AND claim_owner=? AND \
                         lease_epoch=? AND lease_until > ?"
                        [
                          Sqlite3.Data.FLOAT now;
                          int no;
                          int no;
                          opt_text a.relay_ts;
                          text
                            (Yojson.Safe.to_string
                               (`List
                                  (List.map
                                     (fun s -> `String s)
                                     a.uploaded_file_ids)));
                          text (stamp ());
                          text fire_id;
                          text (T.fire_state_to_string r.fire.state);
                          text lease.owner;
                          int lease.lease_epoch;
                          Sqlite3.Data.FLOAT now;
                        ];
                      if Sqlite3.changes db = 1 then Ok true
                      else Error "dispatch reservation lost its lease")))

let suspend_if_epoch db ~order_id ~epoch ~reason ~now =
  guard (fun () ->
      ensure_schema db;
      in_tx db ~on_error:Fun.id (fun () ->
          match find_raw db order_id with
          | Some o when o.state = T.Active && o.epoch = epoch ->
              Result.map
                (fun change -> change.changed)
                (deactivate_in_tx db ~order_id (Suspend reason) ~now)
          | _ -> Ok false))
