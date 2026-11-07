module Sql = Slack_standing_order_sql
(** Message identities and notification reservations survive event/sweep replay.
    No text is persisted. An attempted send is never retried without proof. *)

module T = Slack_standing_order_types
module Rollout = Slack_standing_order_rollout_store

type message = { ts : string; thread_ts : string; user_id : string }

let ensure_schema db =
  Sql_util.exec_schema ~label:"pause_replay" db
    "CREATE TABLE IF NOT EXISTS slack_order_pause_replays (installation TEXT \
     NOT NULL,order_id TEXT NOT NULL,channel TEXT NOT NULL,message_ts TEXT NOT \
     NULL,thread_ts TEXT NOT NULL,user_id TEXT NOT NULL,status TEXT NOT \
     NULL,outcome TEXT,PRIMARY KEY(installation,order_id,channel,message_ts))"

let installation db =
  let _, uuid, _, _ = Rollout.read_anchor db in
  uuid

let identity db (o : T.order) m =
  Sql.[ text (installation db); text o.channel_id; text m.ts; text o.id ]

let command_params db (o : T.order) m =
  Sql.[ text (installation db); text o.id; text o.channel_id; text m.ts ]

let remember db (o : T.order) m =
  ensure_schema db;
  if
    not
      (Slack_standing_order_pages.valid_ts m.ts
      && Slack_standing_order_pages.valid_ts m.thread_ts
      && Slack_config_types.valid_id "UW" m.user_id)
  then invalid_arg "invalid pause replay identity";
  Sql.exec db
    "INSERT INTO slack_order_pause_replays VALUES(?,?,?,?,?,?,'pending',NULL) \
     ON CONFLICT(installation,order_id,channel,message_ts) DO NOTHING"
    (command_params db o m @ Sql.[ text m.thread_ts; text m.user_id ]);
  match
    Sql.query db
      "SELECT thread_ts,user_id,status FROM slack_order_pause_replays WHERE \
       installation=? AND order_id=? AND channel=? AND message_ts=?"
      (command_params db o m) (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2 ))
  with
  | [ (thread, user, status) ] when thread = m.thread_ts && user = m.user_id ->
      status
  | _ -> failwith "conflicting pause message identity"

let outcome db o m code =
  Sql.exec db
    "UPDATE slack_order_pause_replays SET status='complete',outcome=? WHERE \
     installation=? AND order_id=? AND channel=? AND message_ts=? AND \
     status='pending'"
    (Sql.text code :: command_params db o m)

let completed_outcome db o m =
  Sql.query db
    "SELECT outcome FROM slack_order_pause_replays WHERE installation=? AND \
     order_id=? AND channel=? AND message_ts=? AND status='complete'"
    (command_params db o m) (fun s -> Sql_util.text_column s 0)
  |> Sql.first

let notification_status db o m kind =
  Sql.query db
    "SELECT status FROM slack_standing_order_pause_receipts WHERE \
     installation=? AND channel=? AND message_ts=? AND order_id=? AND kind=?"
    (identity db o m @ [ Sql.text kind ])
    (fun s -> Sql_util.text_column s 0)
  |> Sql.first

let reserve db o m kind =
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        Sql.exec db
          "INSERT INTO \
           slack_standing_order_pause_receipts(installation,channel,message_ts,order_id,kind,status) \
           VALUES(?,?,?,?,?,'pending') ON CONFLICT DO NOTHING"
          (identity db o m @ [ Sql.text kind ]);
        Sql.exec db
          "UPDATE slack_standing_order_pause_receipts SET status='attempted' \
           WHERE installation=? AND channel=? AND message_ts=? AND order_id=? \
           AND kind=? AND status='pending'"
          (identity db o m @ [ Sql.text kind ]);
        Ok (Sqlite3.changes db = 1))
  with
  | Ok reserved -> reserved
  | Error e -> failwith e

let finish db o m kind status receipt_ts =
  Sql.exec db
    "UPDATE slack_standing_order_pause_receipts SET status=?,receipt_ts=? \
     WHERE installation=? AND channel=? AND message_ts=? AND order_id=? AND \
     kind=? AND status='attempted'"
    (Sql.[ text status ]
    @ [ Sqlite3.Data.opt_text receipt_ts ]
    @ identity db o m
    @ [ Sql.text kind ])

let marker db (o : T.order) m kind =
  "clawq-pause/1 "
  ^ Digest.to_hex
      (Digest.string
         (String.concat "\000"
            [ installation db; o.T.id; o.channel_id; m.ts; kind ]))

let messages db (o : T.order) =
  ensure_schema db;
  Sql.query db
    "SELECT message_ts,thread_ts,user_id FROM slack_order_pause_replays WHERE \
     installation=? AND order_id=? AND channel=? AND status='pending'"
    Sql.[ text (installation db); text o.id; text o.channel_id ]
    (fun s ->
      {
        ts = Sql_util.text_column s 0;
        thread_ts = Sql_util.text_column s 1;
        user_id = Sql_util.text_column s 2;
      })
