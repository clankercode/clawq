include Slack_standing_order_sql
(** Low-level feature schema and recognized pre-P4 structural checks. *)

let tables =
  [
    ( "slack_standing_order_feature_version",
      "singleton INTEGER PRIMARY KEY CHECK(singleton=1),version INTEGER NOT \
       NULL CHECK(version=1),installation TEXT NOT NULL,bootstrap_digest TEXT \
       NOT NULL,fingerprint TEXT NOT NULL" );
    ( "slack_standing_order_rollout_evidence",
      "seq INTEGER PRIMARY KEY,installation TEXT NOT NULL,version INTEGER NOT \
       NULL,at REAL NOT NULL,kind TEXT NOT NULL,payload TEXT NOT NULL,previous \
       TEXT NOT NULL,digest TEXT NOT NULL UNIQUE" );
    ( "slack_standing_order_rollout_state",
      "singleton INTEGER PRIMARY KEY CHECK(singleton=1),state TEXT NOT \
       NULL,seq INTEGER NOT NULL,identity TEXT,identity_valid INTEGER NOT NULL \
       DEFAULT 0,manifest TEXT" );
    ( "slack_standing_order_order_bindings",
      "order_id TEXT PRIMARY KEY,installation TEXT NOT NULL,identity TEXT,seq \
       INTEGER NOT NULL" );
    ( "slack_standing_order_activation_bindings",
      "order_id TEXT NOT NULL,epoch INTEGER NOT NULL,active_since TEXT NOT \
       NULL,identity TEXT NOT NULL,seq INTEGER NOT NULL,PRIMARY \
       KEY(order_id,epoch)" );
    ( "slack_standing_order_recovery_gaps",
      "installation TEXT NOT NULL,team TEXT NOT NULL,app TEXT NOT NULL,channel \
       TEXT NOT NULL,generation INTEGER NOT NULL,connected INTEGER NOT \
       NULL,cause TEXT NOT NULL,closed_at REAL NOT NULL,PRIMARY \
       KEY(installation,team,app,channel)" );
    ( "slack_standing_order_recovery_passes",
      "order_id TEXT PRIMARY KEY,owner TEXT NOT NULL,generation INTEGER NOT \
       NULL,until REAL NOT NULL,epoch INTEGER NOT NULL,team TEXT NOT NULL,app \
       TEXT NOT NULL,channel TEXT NOT NULL,gap_generation INTEGER NOT NULL \
       DEFAULT 0,mode TEXT NOT NULL DEFAULT 'gap',oldest TEXT,latest \
       TEXT,history_cursor TEXT NOT NULL DEFAULT '',pause_cursor TEXT NOT NULL \
       DEFAULT '',phase TEXT NOT NULL DEFAULT 'idle',last_success \
       REAL,verified_generation INTEGER,verified_epoch INTEGER" );
    ( "slack_standing_order_recovery_candidates",
      "order_id TEXT NOT NULL,root_ts TEXT NOT NULL,room_id TEXT NOT \
       NULL,date_start TEXT NOT NULL,epoch INTEGER NOT NULL,fire_id \
       TEXT,batch_id TEXT,origin TEXT NOT NULL DEFAULT 'recovery',PRIMARY \
       KEY(order_id,root_ts,date_start)" );
    ( "slack_standing_order_recovery_batches",
      "id TEXT PRIMARY KEY,order_id TEXT NOT NULL,epoch INTEGER NOT \
       NULL,status TEXT NOT NULL,notice_ts TEXT,card_id TEXT,release_at \
       REAL,gap_generation INTEGER NOT NULL,owner_generation INTEGER NOT NULL"
    );
    ( "slack_standing_order_pause_receipts",
      "installation TEXT NOT NULL,channel TEXT NOT NULL,message_ts TEXT NOT \
       NULL,order_id TEXT NOT NULL,kind TEXT NOT NULL,status TEXT NOT \
       NULL,receipt_ts TEXT,PRIMARY \
       KEY(installation,channel,message_ts,order_id,kind)" );
    ( "slack_standing_order_thread_work",
      "order_id TEXT NOT NULL,generation INTEGER NOT NULL,root_ts TEXT NOT \
       NULL,cursor TEXT NOT NULL DEFAULT '',completed INTEGER NOT NULL DEFAULT \
       0,PRIMARY KEY(order_id,generation,root_ts)" );
    ( "slack_standing_order_suppression",
      "installation TEXT NOT NULL,order_id TEXT NOT NULL,root_ts TEXT NOT \
       NULL,room_id TEXT NOT NULL,date_start TEXT NOT NULL,receipt_digest TEXT \
       NOT NULL UNIQUE,PRIMARY \
       KEY(installation,order_id,root_ts,room_id,date_start)" );
    ( "slack_standing_order_external_deliveries",
      "receipt_digest TEXT PRIMARY KEY,metadata TEXT NOT NULL,automatic_fire \
       TEXT,automatic_attempt INTEGER" );
    ( "slack_standing_order_inventory_manifests",
      "digest TEXT PRIMARY KEY,installation TEXT NOT NULL,manifest TEXT NOT \
       NULL,audit TEXT NOT NULL" );
  ]

let table_exists db name =
  query db "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
    [ text name ]
    (fun s -> Sql_util.text_column s 0)
  = [ name ]

let names db =
  query db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" []
    (fun s -> Sql_util.text_column s 0)

let starts prefix s = String.starts_with ~prefix s

let feature_artifact n =
  starts "slack_standing_order_rollout" n || List.mem_assoc n tables

let work_table n = starts "slack_standing_order" n || starts "slack_order_" n

let base_names =
  [
    "slack_standing_orders";
    "slack_standing_order_fires";
    "slack_standing_order_cards";
    "slack_standing_order_dispatches";
  ]

let required_columns =
  [
    ( "slack_standing_orders",
      columns
        "id display_no team_id enterprise_id api_app_id channel_id \
         grantor_user_id principal_id trigger_kind action_json target_app_id \
         state active_since epoch revision management_channel_id \
         management_thread_ts max_fires_per_day pending_expires_at created_at \
         confirmed_at confirmed_by expires_at paused_by suspended_reason \
         cancelled_by updated_at bot_user_id bot_id spoken_requests \
         card_version" );
    ( "slack_standing_order_fires",
      columns
        "id order_id room_id date_start root_ts canvas_file_id \
         transcript_file_id notes_ready_ts state epoch claim_owner lease_until \
         lease_epoch cancel_requested repost_forbidden nonce uploaded_file_ids \
         relay_ts attempts reposts next_attempt_at dispatched_at reason \
         source_canvas_id source_transcript_id retention_checked_at created_at \
         updated_at" );
    ( "slack_standing_order_cards",
      columns
        "id kind order_id team_id app_id channel_id reply_thread_ts message_ts \
         plan_id plan_digest order_epoch expires_at used_at created_by \
         created_at" );
    ( "slack_standing_order_dispatches",
      columns
        "fire_id attempt_no uploaded_file_ids relay_ts upload_started_at \
         dispatched_at pickup_started_at first_reply_ts rate_limited_at \
         reaction_seen acknowledged_at picked_up_at not_delivered" );
  ]

let columns_present db table required =
  let found =
    query db
      ("PRAGMA table_info(" ^ table ^ ")")
      []
      (fun s -> Sql_util.text_column s 1)
  in
  List.for_all (fun c -> List.mem c found) required

let base_shape db =
  let nonnull table cols =
    let found =
      query db
        ("PRAGMA table_info(" ^ table ^ ")")
        []
        (fun s -> (Sql_util.text_column s 1, Sql_util.int_column s 3))
    in
    List.for_all (fun c -> List.assoc_opt c found = Some 1) cols
  in
  let unique table cols =
    query db
      ("PRAGMA index_list(" ^ table ^ ")")
      []
      (fun s ->
        ( Sql_util.text_column s 1,
          Sql_util.int_column s 2,
          Sql_util.int_column s 4 ))
    |> List.exists (fun (name, unique, partial) ->
        unique = 1 && partial = 0
        && query db "SELECT name FROM pragma_index_info(?) ORDER BY seqno"
             [ text name ]
             (fun s -> Sql_util.text_column s 0)
           = cols)
  in
  List.for_all
    (fun (table, cols) -> columns_present db table cols)
    required_columns
  && List.for_all
       (fun (table, cols) -> nonnull table cols)
       [
         ( "slack_standing_orders",
           columns "team_id api_app_id channel_id state epoch" );
         ( "slack_standing_order_fires",
           columns "order_id room_id date_start state epoch" );
         ( "slack_standing_order_cards",
           columns "order_id team_id app_id channel_id order_epoch" );
         ("slack_standing_order_dispatches", columns "fire_id attempt_no");
       ]
  && List.for_all
       (fun (table, cols) -> unique table cols)
       [
         ("slack_standing_orders", [ "id" ]);
         ("slack_standing_orders", columns "team_id channel_id display_no");
         ("slack_standing_order_fires", [ "id" ]);
         ("slack_standing_order_fires", columns "order_id room_id date_start");
         ("slack_standing_order_fires", columns "order_id canvas_file_id");
         ("slack_standing_order_cards", [ "id" ]);
         ("slack_standing_order_dispatches", columns "fire_id attempt_no");
       ]
