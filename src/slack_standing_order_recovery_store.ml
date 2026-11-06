module Sql = Slack_standing_order_sql
(** Durable discovery is metadata, never grant authority. All fences are
    synchronous; callers must recheck after their final asynchronous wait. *)

module Store = Slack_standing_order_store
module T = Slack_standing_order_types
module Rollout = Slack_standing_order_rollout_store

type rollout_state = Rollout.rollout_state =
  | Unused_surface_unbound
  | Fresh_no_legacy_work
  | Legacy_inventory_pending
  | Inventory_verified

type rollout_denial = Rollout.rollout_denial =
  | Unbound_installation
  | Inventory_pending
  | Missing_proof
  | Invalid_proof
  | Identity_mismatch
  | Activation_unbound
  | Storage_error

type rollout_readiness = Rollout.rollout_readiness =
  | Ready of rollout_state
  | Denied of rollout_denial

let rollout_readiness = Rollout.rollout_readiness
let readiness_name = Rollout.readiness_name
let suppressed = Rollout.suppressed

type owner = { owner_id : string; generation : int; until : float }
type scan_mode = Gap_recovery | Healthy_maintenance

type gap_token = {
  team_id : string;
  app_id : string;
  channel_id : string;
  gap_generation : int;
}

type coverage = {
  oldest : string;
  latest : string;
  history_cursor : string;
  pause_cursor : string;
  last_success : float option;
}

type batch = {
  id : string;
  order_id : string;
  epoch : int;
  notice_ts : string option;
  release_at : float option;
}

let ensure_schema = Store.ensure_schema

(* File identity is shared across independent handles and aliases. Anonymous
   in-memory databases remain distinct even if copied installation UUIDs match.
   This process-local barrier cannot notify another process after a failed write;
   lifecycle callers must keep it stopped until durable closure succeeds. *)
type database_identity = File of int * int * string | Anonymous of Sqlite3.db

module Database_keys = Ephemeron.K1.Make (struct
  type t = Sqlite3.db

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let database_keys = Database_keys.create 8

let read_database_identity db =
  match
    Sql.query db "PRAGMA database_list" [] (fun s ->
        (Sql_util.text_column s 1, Sql_util.text_column s 2))
    |> List.assoc_opt "main"
  with
  | Some path when path <> "" ->
      let stat =
        try Unix.stat path
        with Unix.Unix_error _ -> failwith "cannot identify recovery database"
      in
      let _, uuid, _, _ = Rollout.read_anchor db in
      File (stat.Unix.st_dev, stat.Unix.st_ino, uuid)
  | _ -> Anonymous db

(* Cache only physical database identity, never rollout/config authority. The
   first admission resolves it; subsequent closure cannot depend on a new read. *)
let database_identity db =
  match Database_keys.find_opt database_keys db with
  | Some identity -> identity
  | None ->
      let identity = read_database_identity db in
      Database_keys.replace database_keys db identity;
      identity

let same_database a b =
  match (a, b) with
  | File (ad, ai, au), File (bd, bi, bu) -> ad = bd && ai = bi && au = bu
  | Anonymous a, Anonymous b -> a == b
  | _ -> false

(* Keep unresolved handles too: a failed identity read must not erase denial
   when the lock clears. Resolving one later shares its denial with other handles.
   Unresolvable identity conservatively denies through the existing DB fences. *)
let local_denied : (Sqlite3.db * string * string * string) list ref = ref []

let same_scope (t : gap_token) (_, team, app, ch) =
  team = t.team_id && app = t.app_id && ch = t.channel_id

let same_handle db t ((other, _, _, _) as entry) =
  db == other && same_scope t entry

let matches identity t ((other, _, _, _) as entry) =
  same_scope t entry && same_database identity (database_identity other)

let latched db token =
  List.exists (same_handle db token) !local_denied
  ||
  let identity = database_identity db in
  List.exists (matches identity token) !local_denied

let latch db (t : gap_token) =
  if not (List.exists (same_handle db t) !local_denied) then
    local_denied := (db, t.team_id, t.app_id, t.channel_id) :: !local_denied

let clear_latch identity token =
  local_denied :=
    List.filter
      (fun entry ->
        try not (matches identity token entry)
        with exn when Sql.is_db_error exn -> true)
      !local_denied

let f n = Sqlite3.Data.FLOAT n

let ready db order_id =
  match rollout_readiness db ~order_id with Ready _ -> true | _ -> false

let attempt_bool fn =
  try fn () with
  | Unix.Unix_error _ -> false
  | exn when Sql.is_db_error exn -> false

let tx_bool db fn =
  match Sql.in_tx db ~on_error:Fun.id (fun () -> Ok (fn ())) with
  | Ok b -> b
  | Error _ -> false

let installation db =
  let _, uuid, _, _ = Rollout.read_anchor db in
  uuid

let gap_params db (t : gap_token) =
  Sql.
    [ text (installation db); text t.team_id; text t.app_id; text t.channel_id ]

let gap_where = "installation=? AND team=? AND app=? AND channel=?"

let close_gap db ~team_id ~app_id ~channel_id ~cause ~now =
  let token = { team_id; app_id; channel_id; gap_generation = 0 } in
  latch db token;
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        Rollout.require_initialized db;
        let identity = database_identity db in
        let params = gap_params db token in
        Sql.exec db
          "INSERT INTO \
           slack_standing_order_recovery_gaps(installation,team,app,channel,generation,connected,cause,closed_at) \
           VALUES(?,?,?,?,1,0,?,?) ON CONFLICT(installation,team,app,channel) \
           DO UPDATE SET \
           generation=generation+1,connected=0,cause=excluded.cause,closed_at=excluded.closed_at"
          (params @ [ Sql.text cause; f now ]);
        let generation =
          Sql.int_query db
            ("SELECT generation FROM slack_standing_order_recovery_gaps WHERE "
           ^ gap_where)
            params
        in
        Ok ({ token with gap_generation = generation }, identity))
  with
  | Error e -> Error e
  | Ok (token, identity) ->
      clear_latch identity token;
      Ok token

let current_gap db (t : gap_token) ~connected =
  (not (latched db t))
  && Sql.int_query db
       ("SELECT count(*) FROM slack_standing_order_recovery_gaps WHERE "
      ^ gap_where ^ " AND generation=?"
       ^ if connected then " AND connected=1" else "")
       (gap_params db t @ [ Sql.int t.gap_generation ])
     = 1

let mark_connected db ~token =
  attempt_bool (fun () ->
      if latched db token then false
      else (
        Sql.exec db
          ("UPDATE slack_standing_order_recovery_gaps SET connected=1 WHERE "
         ^ gap_where ^ " AND generation=?")
          (gap_params db token @ [ Sql.int token.gap_generation ]);
        Sqlite3.changes db = 1))

let claim db ~order_id ~owner_id ~now =
  try
    match
      Sql.in_tx db ~on_error:Fun.id (fun () ->
          let o = Store.find db order_id in
          match o with
          | None -> Ok None
          | Some (o : T.order) when o.state <> T.Active -> Ok None
          | Some o ->
              Sql.exec db
                "INSERT INTO \
                 slack_standing_order_recovery_passes(order_id,owner,generation,until,epoch,team,app,channel) \
                 VALUES(?,?,1,?,?,?,?,?) ON CONFLICT(order_id) DO UPDATE SET \
                 owner=excluded.owner,generation=generation+1,until=excluded.until,phase=CASE \
                 WHEN epoch<>excluded.epoch THEN 'idle' WHEN phase='complete' \
                 THEN phase ELSE 'pause' \
                 END,history_cursor='',pause_cursor='',epoch=excluded.epoch \
                 WHERE until<=?"
                Sql.
                  [
                    text order_id;
                    text owner_id;
                    f (now +. 120.);
                    int o.epoch;
                    text o.team_id;
                    text o.api_app_id;
                    text o.channel_id;
                    f now;
                  ];
              if Sqlite3.changes db <> 1 then Ok None
              else
                let generation =
                  Sql.int_query db
                    "SELECT generation FROM \
                     slack_standing_order_recovery_passes WHERE order_id=?"
                    [ Sql.text order_id ]
                in
                Ok (Some { owner_id; generation; until = now +. 120. }))
    with
    | Ok o -> o
    | Error _ -> None
  with exn when Sql.is_db_error exn -> None

let owner_params order_id (o : owner) now =
  Sql.[ text order_id; text o.owner_id; int o.generation; f now ]

let owner_where =
  "p.order_id=? AND p.owner=? AND p.generation=? AND p.until>? AND \
   o.id=p.order_id AND o.state='active' AND o.epoch=p.epoch AND \
   o.team_id=p.team AND o.api_app_id=p.app AND o.channel_id=p.channel"

let owns db ~order_id ~owner ~now =
  Sql.int_query db
    ("SELECT count(*) FROM slack_standing_order_recovery_passes p JOIN \
      slack_standing_orders o ON o.id=p.order_id WHERE " ^ owner_where)
    (owner_params order_id owner now)
  = 1

let pass_token db order_id =
  match
    Sql.query db
      "SELECT team,app,channel,gap_generation FROM \
       slack_standing_order_recovery_passes WHERE order_id=?"
      [ Sql.text order_id ]
      (fun s ->
        {
          team_id = Sql_util.text_column s 0;
          app_id = Sql_util.text_column s 1;
          channel_id = Sql_util.text_column s 2;
          gap_generation = Sql_util.int_column s 3;
        })
  with
  | [ t ] -> t
  | _ -> failwith "missing recovery pass"

let fenced db ~order_id ~owner ~now =
  owns db ~order_id ~owner ~now
  && current_gap db (pass_token db order_id) ~connected:true

let renew db ~order_id ~owner ~now =
  attempt_bool (fun () ->
      if not (owns db ~order_id ~owner ~now) then false
      else (
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET until=? WHERE \
           order_id=? AND owner=? AND generation=? AND until>?"
          (f (now +. 120.) :: owner_params order_id owner now);
        Sqlite3.changes db = 1))

let load_coverage db ~order_id =
  try
    Sql.query db
      "SELECT oldest,latest,history_cursor,pause_cursor,last_success FROM \
       slack_standing_order_recovery_passes WHERE order_id=? AND oldest IS NOT \
       NULL AND latest IS NOT NULL"
      [ Sql.text order_id ]
      (fun s ->
        {
          oldest = Sql_util.text_column s 0;
          latest = Sql_util.text_column s 1;
          history_cursor = Sql_util.text_column s 2;
          pause_cursor = Sql_util.text_column s 3;
          last_success = Sql.opt_float_column s 4;
        })
    |> Sql.first
  with exn when Sql.is_db_error exn -> None

let begin_scan db ~(order : T.order) ~owner ~mode ~gap ~oldest ~latest ~now =
  tx_bool db (fun () ->
      if
        (not (T.valid_slack_ts oldest && T.valid_slack_ts latest))
        || (not (owns db ~order_id:order.id ~owner ~now))
        || (not (current_gap db gap ~connected:true))
        || gap.team_id <> order.team_id
        || gap.app_id <> order.api_app_id
        || gap.channel_id <> order.channel_id
      then false
      else (
        (* Incomplete scans resume their SAME bounds, including after takeover.
           A changed gap invalidates cursors but never discards candidates. *)
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET oldest=CASE WHEN \
           phase IN ('idle','complete') OR gap_generation<>? THEN ? ELSE \
           oldest END,latest=CASE WHEN phase IN ('idle','complete') OR \
           gap_generation<>? THEN ? ELSE latest END,history_cursor=CASE WHEN \
           phase IN ('idle','complete') OR gap_generation<>? THEN '' ELSE \
           history_cursor END,pause_cursor=CASE WHEN phase IN \
           ('idle','complete') OR gap_generation<>? THEN '' ELSE pause_cursor \
           END,phase=CASE WHEN phase IN ('idle','complete') OR \
           gap_generation<>? THEN 'pause' ELSE phase \
           END,gap_generation=?,mode=? WHERE order_id=?"
          Sql.
            [
              int gap.gap_generation;
              text oldest;
              int gap.gap_generation;
              text latest;
              int gap.gap_generation;
              int gap.gap_generation;
              int gap.gap_generation;
              int gap.gap_generation;
              text
                (match mode with
                | Gap_recovery -> "gap"
                | Healthy_maintenance -> "maintenance");
              text order.id;
            ];
        true))

let save_page db ~order_id ~owner ~(coverage : coverage) ~candidates ~now =
  tx_bool db (fun () ->
      if not (fenced db ~order_id ~owner ~now) then false
      else
        match load_coverage db ~order_id with
        | None -> false
        | Some previous
          when previous.oldest <> coverage.oldest
               || previous.latest <> coverage.latest ->
            false
        | Some previous
          when let invalid s =
                 String.length s > 1024
                 || String.exists (fun c -> Char.code c < 32) s
               in
               invalid coverage.history_cursor
               || invalid coverage.pause_cursor
               || coverage.history_cursor <> ""
                  && coverage.history_cursor = previous.history_cursor
               || coverage.pause_cursor <> ""
                  && coverage.pause_cursor = previous.pause_cursor ->
            Sql.exec db
              "UPDATE slack_standing_order_recovery_passes SET \
               history_cursor='',pause_cursor='',phase='pause' WHERE \
               order_id=?"
              [ Sql.text order_id ];
            false
        | Some _ ->
            let epoch =
              Sql.int_query db
                "SELECT epoch FROM slack_standing_orders WHERE id=?"
                [ Sql.text order_id ]
            in
            List.iter
              (fun (root, room, date_start) ->
                if
                  not
                    (T.valid_slack_ts root
                    && Slack_config_types.valid_id "R" room
                    && T.valid_slack_ts
                         (if String.contains date_start '.' then date_start
                          else date_start ^ ".0"))
                then failwith "invalid candidate metadata";
                let existing =
                  Sql.query db
                    "SELECT room_id FROM \
                     slack_standing_order_recovery_candidates WHERE order_id=? \
                     AND root_ts=? AND date_start=?"
                    Sql.[ text order_id; text root; text date_start ]
                    (fun s -> Sql_util.text_column s 0)
                in
                if existing <> [] && existing <> [ room ] then
                  failwith "conflicting root instance";
                Sql.exec db
                  "INSERT INTO \
                   slack_standing_order_recovery_candidates(order_id,root_ts,room_id,date_start,epoch,fire_id) \
                   VALUES(?,?,?,?,?,(SELECT id FROM slack_standing_order_fires \
                   WHERE order_id=? AND room_id=? AND date_start=?)) ON \
                   CONFLICT(order_id,root_ts,date_start) DO NOTHING"
                  Sql.
                    [
                      text order_id;
                      text root;
                      text room;
                      text date_start;
                      int epoch;
                      text order_id;
                      text room;
                      text date_start;
                    ])
              candidates;
            Sql.exec db
              "UPDATE slack_standing_order_recovery_passes SET \
               history_cursor=?,pause_cursor=? WHERE order_id=?"
              Sql.
                [
                  text coverage.history_cursor;
                  text coverage.pause_cursor;
                  text order_id;
                ];
            true)

let phase db order_id =
  Sql.query db
    "SELECT phase FROM slack_standing_order_recovery_passes WHERE order_id=?"
    [ Sql.text order_id ]
    (fun s -> Sql_util.text_column s 0)
  |> Sql.first

let complete_pause db ~order_id ~owner ~now =
  tx_bool db (fun () ->
      if not (fenced db ~order_id ~owner ~now) then false
      else (
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET phase='history' \
           WHERE order_id=? AND phase='pause' AND pause_cursor='' AND NOT \
           EXISTS(SELECT 1 FROM slack_standing_order_thread_work WHERE \
           order_id=? AND generation=? AND completed=0)"
          Sql.[ text order_id; text order_id; int owner.generation ];
        Sqlite3.changes db = 1))

let complete_history db ~order_id ~owner ~now =
  tx_bool db (fun () ->
      if not (fenced db ~order_id ~owner ~now) then false
      else (
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET phase='swept' WHERE \
           order_id=? AND phase='history' AND history_cursor=''"
          [ Sql.text order_id ];
        Sqlite3.changes db = 1))

let finish_scan db ~order_id ~owner ~now =
  tx_bool db (fun () ->
      if not (fenced db ~order_id ~owner ~now) then false
      else (
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET phase='complete' \
           WHERE order_id=? AND phase='swept' AND history_cursor='' AND \
           pause_cursor=''"
          [ Sql.text order_id ];
        Sqlite3.changes db = 1))

let verify_gap db ~token ~owner ~through ~now =
  tx_bool db (fun () ->
      let matches =
        Sql.query db
          "SELECT order_id FROM slack_standing_order_recovery_passes WHERE \
           owner=? AND generation=?"
          Sql.[ text owner.owner_id; int owner.generation ]
          (fun s -> Sql_util.text_column s 0)
      in
      match matches with
      | [ order_id ]
        when fenced db ~order_id ~owner ~now
             && pass_token db order_id = token
             && ready db order_id
             && phase db order_id = Some "complete" -> (
          match load_coverage db ~order_id with
          | Some c when c.latest = through ->
              Sql.exec db
                "UPDATE slack_standing_order_recovery_passes SET \
                 verified_generation=gap_generation,verified_epoch=epoch,last_success=? \
                 WHERE order_id=?"
                Sql.[ f now; text order_id ];
              true
          | _ -> false)
      | _ -> false)

let covered db order_id epoch =
  match
    Sql.query db
      "SELECT p.team,p.app,p.channel,p.gap_generation FROM \
       slack_standing_order_recovery_passes p WHERE p.order_id=? AND \
       p.verified_epoch=? AND p.verified_generation=p.gap_generation"
      Sql.[ text order_id; int epoch ]
      (fun s ->
        {
          team_id = Sql_util.text_column s 0;
          app_id = Sql_util.text_column s 1;
          channel_id = Sql_util.text_column s 2;
          gap_generation = Sql_util.int_column s 3;
        })
  with
  | [ t ] -> current_gap db t ~connected:true
  | _ -> false

let relay_allowed db ~fire_id ~now =
  attempt_bool (fun () ->
      match
        Sql.query db
          "SELECT \
           f.order_id,f.root_ts,f.room_id,f.date_start,f.epoch,o.epoch,o.state \
           FROM slack_standing_order_fires f JOIN slack_standing_orders o ON \
           o.id=f.order_id WHERE f.id=?"
          [ Sql.text fire_id ]
          (fun s ->
            ( Sql_util.text_column s 0,
              Sql_util.opt_text_column s 1,
              Sql_util.text_column s 2,
              Sql_util.text_column s 3,
              Sql_util.int_column s 4,
              Sql_util.int_column s 5,
              Sql_util.text_column s 6 ))
      with
      | [
       (order_id, Some root, room, date_start, epoch, current_epoch, "active");
      ]
        when epoch = current_epoch && ready db order_id
             && covered db order_id epoch
             && not
                  (suppressed db ~order_id ~root_ts:root ~room_id:room
                     ~date_start) -> (
          match
            Sql.query db
              "SELECT origin,batch_id FROM \
               slack_standing_order_recovery_candidates WHERE order_id=? AND \
               root_ts=? AND date_start=? AND room_id=? AND epoch=?"
              Sql.
                [
                  text order_id;
                  text root;
                  text date_start;
                  text room;
                  int epoch;
                ]
              (fun s ->
                (Sql_util.text_column s 0, Sql_util.opt_text_column s 1))
          with
          | [ ("live", None) ] -> true
          | [ ("recovery", Some batch_id) ] ->
              Sql.int_query db
                "SELECT count(*) FROM slack_standing_order_recovery_batches b \
                 JOIN slack_standing_order_cards c ON c.id=b.card_id JOIN \
                 slack_standing_orders o ON o.id=b.order_id JOIN \
                 slack_standing_order_recovery_passes p ON p.order_id=o.id \
                 WHERE b.id=? AND b.order_id=? AND b.epoch=? AND \
                 b.status='released' AND b.notice_ts IS NOT NULL AND \
                 b.release_at<=? AND b.gap_generation=p.gap_generation AND \
                 b.owner_generation=p.generation AND p.until>? AND \
                 p.epoch=o.epoch AND c.kind='pause' AND c.order_id=o.id AND \
                 c.team_id=o.team_id AND c.app_id=o.api_app_id AND \
                 c.channel_id=o.channel_id AND c.message_ts=b.notice_ts"
                Sql.[ text batch_id; text order_id; int epoch; f now; f now ]
              = 1
          | _ -> false)
      | _ -> false)

let record_root ?(recovery = false) db ~(order : T.order) ~root_ts ~room_id
    ~date_start ~fire_id =
  (* Persist BEFORE touching the replaceable observation ledger. Absence of
     verified coverage always makes this a held recovery candidate. *)
  let existing =
    Sql.query db
      "SELECT room_id FROM slack_standing_order_recovery_candidates WHERE \
       order_id=? AND root_ts=? AND date_start=?"
      Sql.[ text order.id; text root_ts; text date_start ]
      (fun row -> Sql_util.text_column row 0)
  in
  if existing <> [] && existing <> [ room_id ] then
    failwith "conflicting durable huddle identity";
  let origin =
    if (not recovery) && ready db order.id && covered db order.id order.epoch
    then "live"
    else "recovery"
  in
  Sql.exec db
    "INSERT INTO \
     slack_standing_order_recovery_candidates(order_id,root_ts,room_id,date_start,epoch,fire_id,origin) \
     VALUES(?,?,?,?,?,?,?) ON CONFLICT(order_id,root_ts,date_start) DO UPDATE \
     SET fire_id=COALESCE(fire_id,excluded.fire_id) WHERE \
     room_id=excluded.room_id"
    Sql.
      [
        text order.id;
        text root_ts;
        text room_id;
        text date_start;
        int order.epoch;
        Sqlite3.Data.opt_text fire_id;
        text origin;
      ]

let recover_startup db ~now =
  ensure_schema db;
  let channels =
    Sql.query db
      "SELECT DISTINCT team_id,api_app_id,channel_id FROM slack_standing_orders"
      [] (fun s ->
        ( Sql_util.text_column s 0,
          Sql_util.text_column s 1,
          Sql_util.text_column s 2 ))
  in
  List.iter
    (fun (team_id, app_id, channel_id) ->
      match close_gap db ~team_id ~app_id ~channel_id ~cause:"startup" ~now with
      | Ok _ -> ()
      | Error e -> failwith e)
    channels;
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        (* A crash can occur after durable root discovery but before fire
           insertion. Coverage after restart cannot turn that old root live. *)
        Sql.exec db
          "UPDATE slack_standing_order_recovery_candidates AS c SET \
           origin='recovery' WHERE NOT EXISTS(SELECT 1 FROM \
           slack_standing_order_fires f WHERE f.order_id=c.order_id AND \
           f.room_id=c.room_id AND f.date_start=c.date_start AND \
           f.dispatched_at IS NOT NULL)"
          [];
        Sql.exec db
          "INSERT INTO \
           slack_standing_order_recovery_candidates(order_id,root_ts,room_id,date_start,epoch,fire_id) \
           SELECT order_id,root_ts,room_id,date_start,epoch,id FROM \
           slack_standing_order_fires WHERE root_ts IS NOT NULL AND \
           dispatched_at IS NULL AND state IN \
           ('detected','fetching','uploading') ON \
           CONFLICT(order_id,root_ts,date_start) DO UPDATE SET \
           origin='recovery',fire_id=excluded.fire_id"
          [];
        Sql.exec db
          "UPDATE slack_standing_order_fires SET \
           reason='p4_recovery_held',next_attempt_at=NULL WHERE dispatched_at \
           IS NULL AND state IN ('detected','fetching','uploading') AND \
           (claim_owner IS NULL OR lease_until<=?)"
          [ f now ];
        Sql.exec db
          "UPDATE slack_standing_order_recovery_passes SET until=0,phase='idle'"
          [];
        Ok ())
  with
  | Ok () -> ()
  | Error e -> failwith e

let suppression_count = Slack_standing_order_recovery_view.suppression_count
let external_statuses = Slack_standing_order_recovery_view.external_statuses
let status_json = Slack_standing_order_recovery_view.status_json
let report_statuses = Slack_standing_order_recovery_view.report_statuses

(* Store-only batch operations. Posting/adoption and orchestration belong to
   the catch-up task; these CAS operations never send a notice. *)
let find_batch db ~batch_id =
  try
    Sql.query db
      "SELECT id,order_id,epoch,notice_ts,release_at FROM \
       slack_standing_order_recovery_batches WHERE id=?"
      [ Sql.text batch_id ]
      (fun s ->
        {
          id = Sql_util.text_column s 0;
          order_id = Sql_util.text_column s 1;
          epoch = Sql_util.int_column s 2;
          notice_ts = Sql_util.opt_text_column s 3;
          release_at = Sql.opt_float_column s 4;
        })
    |> Sql.first
  with exn when Sql.is_db_error exn -> None

let create_batch db ~(order : T.order) ~owner ~batch_id ~card_id ~roots ~now =
  tx_bool db (fun () ->
      if roots = [] || not (fenced db ~order_id:order.id ~owner ~now) then false
      else
        let token = pass_token db order.id in
        Sql.exec db
          "INSERT INTO \
           slack_standing_order_recovery_batches(id,order_id,epoch,status,card_id,gap_generation,owner_generation) \
           VALUES(?,?,?,'pending',?,?,?)"
          Sql.
            [
              text batch_id;
              text order.id;
              int order.epoch;
              text card_id;
              int token.gap_generation;
              int owner.generation;
            ];
        List.iter
          (fun (root, start) ->
            Sql.exec db
              "UPDATE slack_standing_order_recovery_candidates SET batch_id=? \
               WHERE order_id=? AND epoch=? AND root_ts=? AND date_start=? AND \
               batch_id IS NULL AND origin='recovery'"
              Sql.
                [
                  text batch_id;
                  text order.id;
                  int order.epoch;
                  text root;
                  text start;
                ];
            if Sqlite3.changes db <> 1 then failwith "batch candidate changed")
          roots;
        true)

let batch_fenced db (b : batch) owner now =
  fenced db ~order_id:b.order_id ~owner ~now
  && Sql.int_query db
       "SELECT count(*) FROM slack_standing_order_recovery_batches b JOIN \
        slack_standing_order_recovery_passes p ON p.order_id=b.order_id WHERE \
        b.id=? AND b.epoch=p.epoch AND b.gap_generation=p.gap_generation AND \
        b.owner_generation=p.generation"
       [ Sql.text b.id ]
     = 1

let reserve_notice db ~batch_id ~owner ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b when batch_fenced db b owner now ->
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             status='notice_attempted' WHERE id=? AND status='pending'"
            [ Sql.text batch_id ];
          Sqlite3.changes db = 1
      | _ -> false)

let bind_notice db ~batch_id ~owner ~ts ~release_at ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when T.valid_slack_ts ts && Float.is_finite release_at
             && release_at >= now +. 600.
             && batch_fenced db b owner now -> (
          let card =
            Sql.query db
              "SELECT c.id FROM slack_standing_order_recovery_batches b JOIN \
               slack_standing_order_cards c ON c.id=b.card_id JOIN \
               slack_standing_orders o ON o.id=b.order_id WHERE b.id=? AND \
               b.status IN ('notice_attempted','notice_unknown') AND \
               c.kind='pause' AND c.order_id=o.id AND c.team_id=o.team_id AND \
               c.app_id=o.api_app_id AND c.channel_id=o.channel_id AND \
               c.order_epoch=o.epoch AND (c.message_ts IS NULL OR \
               c.message_ts=?)"
              Sql.[ text batch_id; text ts ]
              (fun s -> Sql_util.text_column s 0)
          in
          match card with
          | [ card_id ] ->
              Sql.exec db
                "UPDATE slack_standing_order_cards SET message_ts=? WHERE id=?"
                Sql.[ text ts; text card_id ];
              Sql.exec db
                "UPDATE slack_standing_order_recovery_batches SET \
                 status='confirmed',notice_ts=?,release_at=? WHERE id=?"
                Sql.[ text ts; f release_at; text batch_id ];
              true
          | _ -> false)
      | _ -> false)

let release_batch db ~batch_id ~owner ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when batch_fenced db b owner now
             && ready db b.order_id
             && covered db b.order_id b.epoch ->
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             status='released' WHERE id=? AND status='confirmed' AND notice_ts \
             IS NOT NULL AND release_at<=? AND EXISTS(SELECT 1 FROM \
             slack_standing_order_cards c WHERE c.id=card_id AND \
             c.message_ts=notice_ts AND c.order_epoch=epoch)"
            Sql.[ text batch_id; f now ];
          Sqlite3.changes db = 1
      | _ -> false)

let adopt_batch_owner db ~batch_id ~owner ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when fenced db ~order_id:b.order_id ~owner ~now
             && ready db b.order_id
             && covered db b.order_id b.epoch ->
          let token = pass_token db b.order_id in
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             owner_generation=?,gap_generation=? WHERE id=? AND epoch=? AND \
             status IN \
             ('pending','notice_attempted','notice_unknown','confirmed','released')"
            Sql.
              [
                int owner.generation;
                int token.gap_generation;
                text batch_id;
                int b.epoch;
              ];
          Sqlite3.changes db = 1
      | _ -> false)

let save_thread db ~order_id ~owner ~root_ts ~cursor ~completed ~now =
  tx_bool db (fun () ->
      if
        (not (fenced db ~order_id ~owner ~now))
        || not (T.valid_slack_ts root_ts)
      then false
      else (
        Sql.exec db
          "INSERT INTO slack_standing_order_thread_work VALUES(?,?,?,?,?) ON \
           CONFLICT(order_id,generation,root_ts) DO UPDATE SET \
           cursor=excluded.cursor,completed=excluded.completed"
          Sql.
            [
              text order_id;
              int owner.generation;
              text root_ts;
              text cursor;
              bool completed;
            ];
        true))

let list_fires_readonly db ~order_id ~limit =
  if not (Rollout.table_exists db "slack_standing_order_fires") then []
  else
    Store.query_fires db
      "WHERE order_id=? ORDER BY created_at DESC,rowid DESC LIMIT ?"
      Sql.[ text order_id; int (max 0 limit) ]

(* Pause's fixed post-commit operations retain the original pass owner. This
   narrowly tolerates that pass's pause epoch; resume or any other transition
   invalidates it. It is never used for discovery, coverage or relay admission. *)
let replay_fenced db ~(order : T.order) ~owner ~now =
  attempt_bool (fun () ->
      current_gap db (pass_token db order.id) ~connected:true
      && Sql.int_query db
           "SELECT count(*) FROM slack_standing_order_recovery_passes p JOIN \
            slack_standing_orders o ON o.id=p.order_id WHERE p.order_id=? AND \
            p.owner=? AND p.generation=? AND p.until>? AND p.epoch=? AND \
            o.team_id=p.team AND o.api_app_id=p.app AND o.channel_id=p.channel \
            AND ((o.state='active' AND o.epoch=p.epoch) OR (o.state IN \
            ('paused','suspended') AND o.epoch=p.epoch+1))"
           (owner_params order.id owner now @ [ Sql.int order.epoch ])
         = 1)
