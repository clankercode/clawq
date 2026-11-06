module Sql = Slack_standing_order_sql
(** Durable inspection units supplement the fixed Task 3 coverage contract. The
    containing page is completed only after every unit succeeds. *)

module R = Slack_standing_order_recovery_store
module T = Slack_standing_order_types
module L = Slack_huddle_ledger
module Q = Slack_standing_order_schedule

let ensure_schema db =
  List.iter
    (Sql_util.exec_schema ~label:"reconcile_scan" db)
    [
      "CREATE TABLE IF NOT EXISTS slack_order_scan_units (order_id TEXT NOT \
       NULL,epoch INTEGER NOT NULL,oldest TEXT NOT NULL,latest TEXT NOT \
       NULL,gap INTEGER NOT NULL,kind TEXT NOT NULL,unit TEXT NOT NULL,outcome \
       TEXT NOT NULL,PRIMARY KEY(order_id,epoch,oldest,latest,gap,kind,unit))";
      "CREATE TABLE IF NOT EXISTS slack_order_scan_status (order_id TEXT \
       PRIMARY KEY,method TEXT NOT NULL,code TEXT NOT NULL,resume_unit TEXT \
       NOT NULL,coverage_limit TEXT NOT NULL)";
    ]

let params db (o : T.order) (c : R.coverage) kind unit =
  let gap = R.pass_token db o.id in
  Sql.
    [
      text o.id;
      int o.epoch;
      text c.oldest;
      text c.latest;
      int gap.gap_generation;
      text kind;
      text unit;
    ]

let done_unit db o c kind unit =
  ensure_schema db;
  Sql.int_query db
    "SELECT count(*) FROM slack_order_scan_units WHERE order_id=? AND epoch=? \
     AND oldest=? AND latest=? AND gap=? AND kind=? AND unit=?"
    (params db o c kind unit)
  = 1

let complete_unit db ~(order : T.order) ~owner ~coverage ~kind ~unit ~outcome
    ~now =
  R.tx_bool db (fun () ->
      if not (R.fenced db ~order_id:order.T.id ~owner ~now) then false
      else (
        Sql.exec db
          "INSERT INTO slack_order_scan_units VALUES(?,?,?,?,?,?,?,?) ON \
           CONFLICT DO NOTHING"
          (params db order coverage kind unit @ [ Sql.text outcome ]);
        true))

let status db ~order_id ~method_ ~code ~resume_unit =
  ensure_schema db;
  Sql.exec db
    "INSERT INTO slack_order_scan_status VALUES(?,?,?,?, \
     'unknown_older_threads_outside_bounded_history') ON CONFLICT(order_id) DO \
     UPDATE SET \
     method=excluded.method,code=excluded.code,resume_unit=excluded.resume_unit"
    Sql.[ text order_id; text method_; text code; text resume_unit ]

let threads db (order : T.order) =
  let ledger =
    L.list_rows ~db ~team:order.team_id ~channel:order.channel_id
    |> List.concat_map (fun (r : L.row) ->
        List.filter_map Fun.id
          [ r.root_ts; r.thread_root_ts; r.canvas_thread_ts; r.notes_thread_ts ])
  in
  let durable =
    Sql.query db
      "SELECT root_ts FROM slack_standing_order_recovery_candidates WHERE \
       order_id=? UNION SELECT root_ts FROM slack_standing_order_fires WHERE \
       order_id=? UNION SELECT d.relay_ts FROM slack_standing_order_dispatches \
       d JOIN slack_standing_order_fires f ON f.id=d.fire_id WHERE \
       f.order_id=? UNION SELECT relay_ts FROM slack_standing_order_fires \
       WHERE order_id=?"
      Sql.[ text order.id; text order.id; text order.id; text order.id ]
      (fun s -> Sql_util.opt_text_column s 0)
    |> List.filter_map Fun.id
  in
  List.sort_uniq String.compare (ledger @ durable)

let pending_roots db (order : T.order) =
  let ledger =
    L.list_rows ~db ~team:order.team_id ~channel:order.channel_id
    |> List.filter (fun (r : L.row) -> not r.finalised)
    |> List.filter_map Q.root_of_row
  in
  let durable =
    Sql.query db
      "SELECT c.root_ts FROM slack_standing_order_recovery_candidates c LEFT \
       JOIN slack_standing_order_fires f ON f.id=c.fire_id WHERE c.order_id=? \
       AND c.epoch=? AND (f.id IS NULL OR f.state IN \
       ('detected','fetching','uploading')) UNION SELECT root_ts FROM \
       slack_standing_order_fires WHERE order_id=? AND root_ts IS NOT NULL AND \
       state IN ('detected','fetching','uploading')"
      Sql.[ text order.id; int order.epoch; text order.id ]
      (fun s -> Sql_util.text_column s 0)
  in
  List.sort_uniq String.compare (ledger @ durable)

let thread_work db ~order_id ~owner =
  Sql.query db
    "SELECT root_ts,cursor,completed FROM slack_standing_order_thread_work \
     WHERE order_id=? AND generation=? ORDER BY root_ts"
    Sql.[ text order_id; int owner.R.generation ]
    (fun s ->
      (Sql_util.text_column s 0, Sql_util.text_column s 1, Sql_util.sql_bool s 2))

let parents db (order : T.order) coverage =
  ensure_schema db;
  let gap = R.pass_token db order.T.id in
  Sql.query db
    "SELECT unit FROM slack_order_scan_units WHERE order_id=? AND epoch=? AND \
     oldest=? AND latest=? AND gap=? AND kind='pause_parent'"
    Sql.
      [
        text order.id;
        int order.epoch;
        text coverage.R.oldest;
        text coverage.latest;
        int gap.gap_generation;
      ]
    (fun s -> Sql_util.text_column s 0)

let previous_outcome db (order : T.order) root =
  ensure_schema db;
  Sql.query db
    "SELECT outcome FROM slack_order_scan_units WHERE order_id=? AND epoch=? \
     AND kind='root' AND unit=? ORDER BY rowid DESC LIMIT 1"
    Sql.[ text order.id; int order.epoch; text root ]
    (fun s -> Sql_util.text_column s 0)
  |> Sql.first
