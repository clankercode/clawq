module Sql = Slack_standing_order_sql
module Rollout = Slack_standing_order_rollout_store

(* Only delivery identities and progress are durable. Never store event bodies. *)
let ensure_schema db =
  List.iter
    (Sql_util.exec_schema ~label:"standing_order_admission" db)
    [
      "CREATE TABLE IF NOT EXISTS slack_order_ingress (installation TEXT NOT \
       NULL,team TEXT NOT NULL,app TEXT NOT NULL,channel TEXT NOT \
       NULL,identity TEXT NOT NULL,ts TEXT NOT NULL,pause INTEGER NOT \
       NULL,applied INTEGER NOT NULL DEFAULT 0,PRIMARY \
       KEY(installation,team,app,channel,identity))";
      "CREATE TABLE IF NOT EXISTS slack_order_dispatch_admission (fire_id TEXT \
       NOT NULL,attempt INTEGER NOT NULL,generation INTEGER NOT NULL,PRIMARY \
       KEY(fire_id,attempt))";
      "CREATE TABLE IF NOT EXISTS slack_order_reconcile_retry (order_id TEXT \
       PRIMARY KEY,retry_at REAL NOT NULL DEFAULT 0,failures INTEGER NOT NULL \
       DEFAULT 0)";
      "CREATE TABLE IF NOT EXISTS slack_order_coverage_origin (order_id TEXT \
       PRIMARY KEY,status TEXT NOT NULL)";
      "CREATE TABLE IF NOT EXISTS slack_order_thread_frontier (order_id TEXT \
       NOT NULL,epoch INTEGER NOT NULL,root TEXT NOT NULL,reply_count INTEGER \
       NOT NULL,latest_reply TEXT NOT NULL,through TEXT NOT NULL,PRIMARY \
       KEY(order_id,epoch,root))";
    ]

let scope db ~team ~app ~channel =
  let _, installation, _, _ = Rollout.read_anchor db in
  Sql.[ text installation; text team; text app; text channel ]

let clear db ~team ~app ~channel ?through () =
  let pred, extra =
    match through with
    | None -> ("pause=1", [])
    | Some ts ->
        ("(pause=1 OR CAST(ts AS REAL)<=CAST(? AS REAL))", [ Sql.text ts ])
  in
  Sql.int_query db
    ("SELECT count(*) FROM slack_order_ingress WHERE installation=? AND team=? \
      AND app=? AND channel=? AND applied=0 AND " ^ pred)
    (scope db ~team ~app ~channel @ extra)
  = 0

let reserve db ~team ~app ~channel ~identity ~ts ~pause =
  Sql.exec db
    "INSERT OR IGNORE INTO \
     slack_order_ingress(installation,team,app,channel,identity,ts,pause) \
     VALUES(?,?,?,?,?,?,?)"
    (scope db ~team ~app ~channel @ Sql.[ text identity; text ts; bool pause ])

let applied db ~team ~app ~channel ~identity =
  Sql.exec db
    "UPDATE slack_order_ingress SET applied=1 WHERE installation=? AND team=? \
     AND app=? AND channel=? AND identity=?"
    (scope db ~team ~app ~channel @ [ Sql.text identity ])

let due db order_id now =
  Sql.query db
    "SELECT retry_at FROM slack_order_reconcile_retry WHERE order_id=?"
    [ Sql.text order_id ]
    (fun s -> Sql.opt_float_column s 0)
  |> function
  | [ Some at ] -> at <= now
  | _ -> true

let failed db order_id now =
  Sql.exec db
    "INSERT INTO slack_order_reconcile_retry VALUES(?,?,1) ON \
     CONFLICT(order_id) DO UPDATE SET retry_at=? + CASE failures WHEN 0 THEN \
     120 WHEN 1 THEN 300 WHEN 2 THEN 600 ELSE 1200 END, \
     failures=MIN(4,failures+1)"
    Sql.
      [
        text order_id; Sqlite3.Data.FLOAT (now +. 120.); Sqlite3.Data.FLOAT now;
      ]

let succeeded db order_id =
  Sql.exec db "DELETE FROM slack_order_reconcile_retry WHERE order_id=?"
    [ Sql.text order_id ]

(* Only committed or definitive refused decisions clear a pending pause. A
   completed handler alone is insufficient: Pause deliberately absorbs transient
   membership failures so chat ingress remains available. *)
let settle_pauses db =
  if Rollout.table_exists db "slack_order_pause_replays" then
    Sql.exec db
      "UPDATE slack_order_ingress AS i SET applied=1 WHERE i.pause=1 AND \
       EXISTS(SELECT 1 FROM slack_order_pause_replays p WHERE \
       p.installation=i.installation AND p.channel=i.channel AND \
       'message:'||p.message_ts=i.identity AND p.status='complete')"
      []
