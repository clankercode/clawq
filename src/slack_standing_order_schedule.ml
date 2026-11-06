module Sql = Slack_standing_order_sql
(** Durable metadata scheduling only. A hint never supplies grant authority or
    verified source identity. All mutations finish synchronously before I/O. *)

module Store = Slack_standing_order_store
module T = Slack_standing_order_types
module Grant = Slack_standing_order_grant
module L = Slack_huddle_ledger
module P = Slack_standing_order_provenance

type root_key = {
  team_id : string;
  app_id : string;
  channel_id : string;
  order_id : string;
  epoch : int;
  root_ts : string;
}

type resolution_lease = {
  key : root_key;
  owner_id : string;
  dirty_revision : int;
  until : float;
  lease_epoch : int;
}

type resolution_outcome = Live | Ended | Settled

let ensure_schema db =
  List.iter
    (Sql_util.exec_schema ~label:"standing_order_schedule" db)
    [
      "CREATE TABLE IF NOT EXISTS slack_order_root_schedule (team TEXT NOT \
       NULL, app TEXT NOT NULL, channel TEXT NOT NULL, order_id TEXT NOT NULL, \
       epoch INTEGER NOT NULL, root TEXT NOT NULL, metadata TEXT NOT NULL \
       DEFAULT '', dirty_revision INTEGER NOT NULL DEFAULT 1, \
       inspected_revision INTEGER NOT NULL DEFAULT 0, next_probe_at REAL NOT \
       NULL DEFAULT 0, retry_at REAL NOT NULL DEFAULT 0, retry_count INTEGER \
       NOT NULL DEFAULT 0, retry_class TEXT, settled INTEGER NOT NULL DEFAULT \
       0, owner TEXT, lease_until REAL, lease_epoch INTEGER NOT NULL DEFAULT \
       0, PRIMARY KEY(team,app,channel,order_id,epoch,root))";
      "CREATE TABLE IF NOT EXISTS slack_order_ready_hints (id INTEGER PRIMARY \
       KEY, team TEXT NOT NULL, app TEXT NOT NULL, channel TEXT NOT NULL, \
       order_id TEXT NOT NULL, epoch INTEGER NOT NULL, thread TEXT NOT NULL, \
       canvas TEXT NOT NULL, ready TEXT NOT NULL, \
       UNIQUE(team,app,channel,order_id,epoch,thread,canvas,ready))";
      "CREATE TABLE IF NOT EXISTS slack_order_ready_taken (fire_id TEXT \
       PRIMARY KEY, hint_id INTEGER NOT NULL)";
    ]

let params (k : root_key) =
  Sql.
    [
      text k.team_id;
      text k.app_id;
      text k.channel_id;
      text k.order_id;
      int k.epoch;
      text k.root_ts;
    ]

let where =
  "team=? AND app=? AND channel=? AND order_id=? AND epoch=? AND root=?"

let key (o : T.order) root_ts =
  {
    team_id = o.team_id;
    app_id = o.api_app_id;
    channel_id = o.channel_id;
    order_id = o.id;
    epoch = o.epoch;
    root_ts;
  }

let register ~db ~key ~metadata =
  ensure_schema db;
  Sql.exec db
    "INSERT INTO \
     slack_order_root_schedule(team,app,channel,order_id,epoch,root,metadata) \
     VALUES(?,?,?,?,?,?,?) ON CONFLICT(team,app,channel,order_id,epoch,root) \
     DO UPDATE SET metadata=excluded.metadata,dirty_revision=dirty_revision+1 \
     WHERE metadata<>excluded.metadata AND settled=0"
    (params key @ [ Sql.text metadata ])

let root_of_row (r : L.row) =
  match r.kind with L.Room -> r.root_ts | Notes_stub -> r.notes_thread_ts

let sync ~db (order : T.order) =
  L.list_rows ~db ~team:order.team_id ~channel:order.channel_id
  |> List.iter (fun (r : L.row) ->
      Option.iter
        (fun root ->
          (* Exclude receive times and source: identical hydration/replay and
              unrelated pickup traffic must not dirty resolution. *)
          let metadata =
            Digest.to_hex
              (Digest.string
                 (Marshal.to_string
                    ( r.room_id,
                      r.date_start,
                      r.date_end,
                      r.has_ended,
                      r.root_file_ids,
                      r.attached_file_ids,
                      r.room_transcript_file_id,
                      r.notes_ready_ts,
                      r.canvas_file_id,
                      r.notes_file_ids,
                      r.finalised )
                    []))
          in
          register ~db ~key:(key order root) ~metadata;
          if r.finalised then
            Sql.exec db
              ("UPDATE slack_order_root_schedule SET settled=1 WHERE " ^ where)
              (params (key order root)))
        (root_of_row r))

let dirty_channel ~db ~channel =
  ensure_schema db;
  Sql.exec db
    "UPDATE slack_order_root_schedule SET dirty_revision=dirty_revision+1 \
     WHERE channel=? AND settled=0"
    [ Sql.text channel ]

let active_key =
  " AND EXISTS (SELECT 1 FROM slack_standing_orders o WHERE \
   o.id=slack_order_root_schedule.order_id AND \
   o.team_id=slack_order_root_schedule.team AND \
   o.api_app_id=slack_order_root_schedule.app AND \
   o.channel_id=slack_order_root_schedule.channel AND \
   o.epoch=slack_order_root_schedule.epoch AND o.state='active')"

let due ~db ~key ~now =
  ensure_schema db;
  Sql.query db
    ("SELECT 1 FROM slack_order_root_schedule WHERE " ^ where
   ^ " AND settled=0 AND retry_at<=? AND (dirty_revision>inspected_revision OR \
      next_probe_at<=?) AND (owner IS NULL OR lease_until<=?)" ^ active_key)
    (params key @ Sqlite3.Data.[ FLOAT now; FLOAT now; FLOAT now ])
    ignore
  <> []

let claim_resolution ~db ~key ~owner_id ~now =
  ensure_schema db;
  if owner_id = "" then None
  else
    match
      Sql.in_tx db ~on_error:Fun.id (fun () ->
          Sql.exec db
            "INSERT OR IGNORE INTO \
             slack_order_root_schedule(team,app,channel,order_id,epoch,root) \
             VALUES(?,?,?,?,?,?)"
            (params key);
          if not (due ~db ~key ~now) then Ok None
          else (
            Sql.exec db
              ("UPDATE slack_order_root_schedule SET \
                owner=?,lease_until=?,lease_epoch=lease_epoch+1 WHERE " ^ where
              )
              (Sql.[ text owner_id ]
              @ [ Sqlite3.Data.FLOAT (now +. 120.) ]
              @ params key);
            Ok
              (Sql.query db
                 ("SELECT dirty_revision,lease_epoch FROM \
                   slack_order_root_schedule WHERE " ^ where) (params key)
                 (fun s ->
                   {
                     key;
                     owner_id;
                     dirty_revision = Sql_util.int_column s 0;
                     until = now +. 120.;
                     lease_epoch = Sql_util.int_column s 1;
                   })
              |> Sql.first)))
    with
    | Ok lease -> lease
    | Error e -> failwith e

let owned (lease : resolution_lease) now =
  params lease.key
  @ Sql.[ text lease.owner_id; int lease.lease_epoch ]
  @ Sqlite3.Data.[ FLOAT now; FLOAT lease.until; FLOAT now ]

let owned_where =
  where
  ^ " AND owner=? AND lease_epoch=? AND lease_until>? AND ?>? AND settled=0"
  ^ active_key

let owns_resolution ~db ~lease ~now =
  Sql.query db
    ("SELECT 1 FROM slack_order_root_schedule WHERE " ^ owned_where)
    (owned lease now) ignore
  <> []

let renew_resolution ~db ~(lease : resolution_lease) ~now =
  Sql.exec db
    ("UPDATE slack_order_root_schedule SET lease_until=? WHERE " ^ owned_where)
    ([ Sqlite3.Data.FLOAT (now +. 120.) ] @ owned lease now);
  if Sqlite3.changes db = 1 then Some { lease with until = now +. 120. }
  else None

let finish_resolution ~db ~(lease : resolution_lease) ~outcome ~now =
  ensure_schema db;
  Sql.exec db
    ("UPDATE slack_order_root_schedule SET inspected_revision=?, \
      next_probe_at=?,retry_at=0,retry_count=0,retry_class=NULL,settled=CASE \
      WHEN dirty_revision=? THEN ? ELSE settled \
      END,owner=NULL,lease_until=NULL WHERE " ^ owned_where)
    (Sql.[ int lease.dirty_revision ]
    @ [ Sqlite3.Data.FLOAT (now +. 300.) ]
    @ Sql.[ int lease.dirty_revision; bool (outcome = Settled) ]
    @ owned lease now);
  Sqlite3.changes db = 1

let fail_resolution ~db ~(lease : resolution_lease) ~(failure : Grant.failure)
    ~now =
  ensure_schema db;
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        (match failure.reason with
        | Grant.Feature_disabled ->
            (* Temporary operator posture is neither completion nor a failed
               remote attempt. Preserve independent retries and dirty work. *)
            Sql.exec db
              ("UPDATE slack_order_root_schedule SET \
                owner=NULL,lease_until=NULL WHERE " ^ owned_where)
              (owned lease now)
        | _ ->
            Sql.exec db
              ("UPDATE slack_order_root_schedule SET retry_at=? + CASE \
                retry_count WHEN 0 THEN 120 WHEN 1 THEN 300 WHEN 2 THEN 600 \
                ELSE 1200 \
                END,retry_count=MIN(3,retry_count+1),retry_class=?,settled=?,owner=NULL,lease_until=NULL \
                WHERE " ^ owned_where)
              ([ Sqlite3.Data.FLOAT now ]
              @ Sql.
                  [
                    text (Grant.reason_code failure.reason);
                    bool (failure.cls <> Grant.Transient);
                  ]
              @ owned lease now));
        if Sqlite3.changes db <> 1 then Ok false
        else if failure.cls <> Grant.Hard then Ok true
        else
          (* The owner CAS and lifecycle sweep commit together. Checking
             ownership after releasing the claim would race a successor. *)
          match
            Store.suspend_if_epoch_in_tx db ~order_id:lease.key.order_id
              ~epoch:lease.key.epoch
              ~reason:(Grant.reason_code failure.reason)
              ~now
          with
          | Ok true -> Ok true
          | Ok false -> Error "resolution suspension lost its order epoch"
          | Error _ as error -> error)
  with
  | Ok applied -> applied
  | Error error -> failwith error

(* The compatibility names now explicitly require the full installation/epoch
   key and ownership token. Successful inspection is never an attempted call. *)
let resolution_due = due
let resolved = finish_resolution

let note_ready ~db ~team ~app ~channel ~thread_ts ~canvas_file_id ~ready_ts
    ~now:_ =
  ensure_schema db;
  Store.list db ()
  |> List.iter (fun (o : T.order) ->
      if
        o.state = T.Active && o.team_id = team && o.api_app_id = app
        && o.channel_id = channel
      then (
        Sql.exec db
          "INSERT OR IGNORE INTO \
           slack_order_ready_hints(team,app,channel,order_id,epoch,thread,canvas,ready) \
           VALUES(?,?,?,?,?,?,?,?)"
          Sql.
            [
              text team;
              text app;
              text channel;
              text o.id;
              int o.epoch;
              text thread_ts;
              text canvas_file_id;
              text ready_ts;
            ];
        if Sqlite3.changes db = 1 then
          Sql.exec db
            ("UPDATE slack_order_root_schedule SET \
              dirty_revision=dirty_revision+1 WHERE " ^ where ^ " AND settled=0"
            )
            (params (key o thread_ts))))

let matching ~db ~now (r : Store.fire_runtime) =
  match (Store.find db r.fire.order_id, r.root_ts) with
  | Some o, Some root
    when o.state = T.Active && o.epoch = r.fire.epoch
         && (not r.fire.cancel_requested)
         && r.fire.reason = Some "notes_not_ready"
         && r.fire.dispatched_at = None
         && List.mem r.fire.state T.[ Detected; Fetching ]
         && Option.fold ~none:false
              ~some:(fun active ->
                T.valid_slack_ts active
                && P.compare_ts r.fire.date_start active >= 0)
              o.active_since
         && Option.fold ~none:false ~some:(fun t -> t > now) r.next_attempt_at
    ->
      Sql.query db
        "SELECT h.id,h.ready FROM slack_order_ready_hints h LEFT JOIN \
         slack_order_ready_taken t ON t.fire_id=? WHERE h.team=? AND h.app=? \
         AND h.channel=? AND h.order_id=? AND h.epoch=? AND h.thread=? AND \
         (h.canvas=? OR ? IS NULL) AND h.id>COALESCE(t.hint_id,0) ORDER BY \
         h.id DESC"
        (Sql.
           [
             text r.fire.id;
             text o.team_id;
             text o.api_app_id;
             text o.channel_id;
             text o.id;
             int o.epoch;
             text root;
           ]
        @ Sqlite3.Data.[ opt_text r.canvas_id; opt_text r.canvas_id ])
        (fun s -> (Sql_util.int_column s 0, Sql_util.text_column s 1))
      |> List.find_map (fun (id, ready) ->
          if
            T.valid_slack_ts ready
            && P.compare_ts ready (Option.get o.active_since) >= 0
          then Some id
          else None)
  | _ -> None

let ready_fires ~db ~now =
  ensure_schema db;
  Store.query_fires db
    "WHERE state IN ('detected','fetching') AND reason='notes_not_ready' AND \
     dispatched_at IS NULL AND next_attempt_at>?"
    [ Sqlite3.Data.FLOAT now ]
  |> List.filter_map (fun (f : T.fire) -> Store.runtime db ~fire_id:f.id)
  |> List.filter (fun r -> matching ~db ~now r <> None)

let take_ready ~db ~fire_id ~lease ~now =
  ensure_schema db;
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        match Store.runtime db ~fire_id with
        | Some before when Store.valid_lease lease before now -> (
            match matching ~db ~now before with
            | None -> Ok false
            | Some hint -> (
                let after = { before with Store.next_attempt_at = Some now } in
                match Store.cas_in_tx db ~lease ~before ~after ~now with
                | Ok true ->
                    Sql.exec db
                      "INSERT INTO slack_order_ready_taken VALUES(?,?) ON \
                       CONFLICT(fire_id) DO UPDATE SET \
                       hint_id=excluded.hint_id"
                      Sql.[ text fire_id; int hint ];
                    Ok true
                | other -> other))
        | _ -> Ok false)
  with
  | Ok taken -> taken
  | Error e -> failwith e

(* Maintenance can inspect a settled candidate again at the ordinary cadence;
   it cannot clear transport retry or steal a live resolution owner. *)
let reopen_for_inspection ~db ~key ~now =
  ensure_schema db;
  Sql.exec db
    ("UPDATE slack_order_root_schedule SET \
      settled=0,dirty_revision=dirty_revision+1 WHERE " ^ where
   ^ " AND settled=1 AND retry_at<=? AND next_probe_at<=? AND (owner IS NULL \
      OR lease_until<=?)" ^ active_key)
    (params key
    @ [ Sqlite3.Data.FLOAT now; Sqlite3.Data.FLOAT now; Sqlite3.Data.FLOAT now ]
    )
