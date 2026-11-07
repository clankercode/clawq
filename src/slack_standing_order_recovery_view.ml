module Sql = Slack_standing_order_sql
module Store = Slack_standing_order_store
module Rollout = Slack_standing_order_rollout_store

let rollout_readiness = Rollout.rollout_readiness
let readiness_name = Rollout.readiness_name

let suppression_count db ~order_id =
  try
    Sql.int_query db
      "SELECT count(*) FROM slack_standing_order_suppression WHERE order_id=?"
      [ Sql.text order_id ]
  with exn when Sql.is_db_error exn -> 0

let external_statuses db ~order_id =
  if not (Rollout.table_exists db "slack_standing_order_suppression") then []
  else
    Sql.query db
      "SELECT e.metadata,e.automatic_fire FROM \
       slack_standing_order_suppression s JOIN \
       slack_standing_order_external_deliveries e ON \
       e.receipt_digest=s.receipt_digest WHERE s.order_id=? ORDER BY s.root_ts"
      [ Sql.text order_id ]
      (fun row ->
        let json = Yojson.Safe.from_string (Sql_util.text_column row 0) in
        let fields =
          List.map
            (fun k -> (k, Yojson.Safe.Util.member k json))
            [
              "root_ts";
              "room_id";
              "date_start";
              "relay_ts";
              "own_file_ids";
              "source_canvas_id";
              "source_transcript_id";
              "evidence_digest";
              "reviewer";
            ]
        in
        `Assoc
          (fields
          @ [
              ( "automatic_fire",
                Option.fold ~none:`Null
                  ~some:(fun s -> `String s)
                  (Sql_util.opt_text_column row 1) );
            ]))

let status_json db ~order_id =
  let status = rollout_readiness db ~order_id in
  let held =
    try
      Sql.int_query db
        "SELECT count(*) FROM slack_standing_order_recovery_candidates c JOIN \
         slack_standing_orders o ON o.id=c.order_id LEFT JOIN \
         slack_standing_order_recovery_batches b ON b.id=c.batch_id WHERE \
         c.order_id=? AND c.epoch=o.epoch AND c.origin='recovery' AND \
         (b.status IS NULL OR b.status NOT IN ('released','cancelled')) AND \
         NOT EXISTS(SELECT 1 FROM slack_standing_order_suppression s WHERE \
         s.order_id=c.order_id AND (s.root_ts=c.root_ts OR \
         (s.room_id=c.room_id AND s.date_start=c.date_start)))"
        [ Sql.text order_id ]
    with exn when Sql.is_db_error exn -> 0
  in
  let proof =
    try
      let uuid, state, seq, identity, _, manifest = Rollout.validate_proof db in
      `Assoc
        [
          ("version", `Int 1);
          ("installation", `String uuid);
          ("state", `String state);
          ("evidence_seq", `Int seq);
          ( "identity",
            Option.fold ~none:`Null
              ~some:(fun s -> Yojson.Safe.from_string s)
              identity );
          ( "inventory_digest",
            Option.fold ~none:`Null ~some:(fun s -> `String s) manifest );
        ]
    with _ -> `Null
  in
  let external_deliveries =
    try `List (external_statuses db ~order_id) with _ -> `Null
  in
  let scan =
    if not (Rollout.table_exists db "slack_order_scan_status") then `Null
    else
      match
        Sql.query db
          "SELECT method,code,resume_unit,coverage_limit FROM \
           slack_order_scan_status WHERE order_id=?"
          [ Sql.text order_id ]
          (fun s ->
            `Assoc
              [
                ("method", `String (Sql_util.text_column s 0));
                ("code", `String (Sql_util.text_column s 1));
                ("resume_unit", `String (Sql_util.text_column s 2));
                ("coverage_limit", `String (Sql_util.text_column s 3));
              ])
      with
      | [ v ] -> v
      | _ -> `Null
  in
  `Assoc
    [
      ("scan", scan);
      ("readiness", `String (readiness_name status));
      ("proof", proof);
      ("already_delivered", `Int (suppression_count db ~order_id));
      ("held_candidates", `Int held);
      ("external_deliveries", external_deliveries);
    ]

let report_statuses db ~order_id =
  if not (Rollout.table_exists db "slack_standing_order_reports") then []
  else
    Sql.query db
      "SELECT cause_key,status FROM slack_standing_order_reports WHERE \
       order_id=? ORDER BY cause_key,period_key"
      [ Sql.text order_id ]
      (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
