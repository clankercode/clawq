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
      "CREATE TABLE IF NOT EXISTS slack_order_ingress_delivery (installation \
       TEXT NOT NULL,team TEXT NOT NULL,app TEXT NOT NULL,channel TEXT NOT \
       NULL,identity TEXT NOT NULL,gap_generation INTEGER NOT NULL,serial \
       INTEGER NOT NULL DEFAULT 0,state TEXT NOT NULL DEFAULT \
       'reserved',PRIMARY KEY(installation,team,app,channel,identity))";
      "CREATE TABLE IF NOT EXISTS slack_order_ingress_handlers (id TEXT \
       PRIMARY KEY,installation TEXT NOT NULL,team TEXT NOT NULL,app TEXT NOT \
       NULL,channel TEXT NOT NULL,identity TEXT NOT NULL,serial INTEGER NOT \
       NULL,host TEXT NOT NULL,boot TEXT NOT NULL,pid INTEGER NOT NULL,start \
       TEXT NOT NULL,state TEXT NOT NULL)";
      "CREATE TABLE IF NOT EXISTS slack_order_gap_report_intents (order_id \
       TEXT NOT NULL,watermark TEXT NOT NULL,first TEXT NOT NULL,last TEXT NOT \
       NULL,PRIMARY KEY(order_id,first,last))";
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
      AND app=? AND channel=? AND (applied=0 OR EXISTS(SELECT 1 FROM \
      slack_order_ingress_handlers h WHERE \
      h.installation=slack_order_ingress.installation AND \
      h.team=slack_order_ingress.team AND h.app=slack_order_ingress.app AND \
      h.channel=slack_order_ingress.channel AND \
      h.identity=slack_order_ingress.identity AND h.state NOT IN \
      ('complete','abandoned'))) AND " ^ pred)
    (scope db ~team ~app ~channel @ extra)
  = 0

let transaction db fn =
  match Sql.in_tx db ~on_error:Fun.id (fun () -> Ok (fn ())) with
  | Ok value -> value
  | Error message -> failwith message

let reserve db ~team ~app ~channel ~identity ~ts ~pause =
  transaction db (fun () ->
      let params = scope db ~team ~app ~channel @ [ Sql.text identity ] in
      Sql.exec db
        "INSERT OR IGNORE INTO \
         slack_order_ingress(installation,team,app,channel,identity,ts,pause) \
         VALUES(?,?,?,?,?,?,?)"
        (params @ Sql.[ text ts; bool pause ]);
      if Sqlite3.changes db = 1 then
        Sql.exec db
          "INSERT INTO \
           slack_order_ingress_delivery(installation,team,app,channel,identity,gap_generation) \
           SELECT installation,team,app,channel,?,generation FROM \
           slack_standing_order_recovery_gaps WHERE installation=? AND team=? \
           AND app=? AND channel=?"
          (Sql.text identity :: scope db ~team ~app ~channel))

let applied db ~team ~app ~channel ~identity =
  Sql.exec db
    "UPDATE slack_order_ingress SET applied=1 WHERE installation=? AND team=? \
     AND app=? AND channel=? AND identity=?"
    (scope db ~team ~app ~channel @ [ Sql.text identity ])

(* A handler belongs to its process incarnation, not its Socket connection or
   recovery generation. Refreshing either must not orphan a running handler. *)
let boot_id () =
  try
    let ic = open_in "/proc/sys/kernel/random/boot_id" in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> input_line ic)
  with Sys_error _ -> ""

let active_handlers = Hashtbl.create 16

type handler = { id : string; params : Sqlite3.Data.t list; serial : int }

let start_handler db ~team ~app ~channel ~identity =
  let params = scope db ~team ~app ~channel @ [ Sql.text identity ] in
  let ticket =
    transaction db (fun () ->
        (* A real handler may settle a legacy reservation whose owner is unknown.
         Replay alone must keep that reservation conservative. *)
        Sql.exec db
          "INSERT OR IGNORE INTO \
           slack_order_ingress_delivery(installation,team,app,channel,identity,gap_generation) \
           SELECT installation,team,app,channel,?,generation FROM \
           slack_standing_order_recovery_gaps WHERE installation=? AND team=? \
           AND app=? AND channel=?"
          (Sql.text identity :: scope db ~team ~app ~channel);
        Sql.exec db
          "UPDATE slack_order_ingress_delivery SET \
           serial=serial+1,state='active' WHERE installation=? AND team=? AND \
           app=? AND channel=? AND identity=? AND EXISTS(SELECT 1 FROM \
           slack_order_ingress i WHERE \
           i.installation=slack_order_ingress_delivery.installation AND \
           i.team=slack_order_ingress_delivery.team AND \
           i.app=slack_order_ingress_delivery.app AND \
           i.channel=slack_order_ingress_delivery.channel AND \
           i.identity=slack_order_ingress_delivery.identity AND i.applied=0)"
          params;
        if Sqlite3.changes db = 0 then None
        else
          let serial =
            Sql.int_query db
              "SELECT serial FROM slack_order_ingress_delivery WHERE \
               installation=? AND team=? AND app=? AND channel=? AND \
               identity=?"
              params
          in
          let id = Temp_downloads.generate_token () in
          let pid = Unix.getpid () in
          Sql.exec db
            "INSERT INTO slack_order_ingress_handlers \
             VALUES(?,?,?,?,?,?,?,?,?,?,?,'active')"
            ((Sql.text id :: params)
            @ Sql.
                [
                  int serial;
                  text (Unix.gethostname ());
                  text (boot_id ());
                  int pid;
                  text
                    (Option.value
                       (Session_host_direct.proc_start_token pid)
                       ~default:"");
                ]);
          Some { id; params; serial })
  in
  Option.iter (fun h -> Hashtbl.replace active_handlers h.id ()) ticket;
  ticket

let finish_handler db ticket ~success =
  Option.iter
    (fun h ->
      Fun.protect
        ~finally:(fun () -> Hashtbl.remove active_handlers h.id)
        (fun () ->
          transaction db (fun () ->
              Sql.exec db
                "UPDATE slack_order_ingress_handlers SET state=? WHERE id=? \
                 AND state='active'"
                Sql.
                  [
                    text (if success then "complete" else "abandoned");
                    text h.id;
                  ];
              if Sqlite3.changes db = 1 then (
                Sql.exec db
                  "UPDATE slack_order_ingress_delivery SET state=? WHERE \
                   installation=? AND team=? AND app=? AND channel=? AND \
                   identity=? AND serial=?"
                  (Sql.text (if success then "applied" else "abandoned")
                   :: h.params
                  @ [ Sql.int h.serial ]);
                if success && Sqlite3.changes db = 1 then
                  Sql.exec db
                    "UPDATE slack_order_ingress SET applied=1 WHERE \
                     installation=? AND team=? AND app=? AND channel=? AND \
                     identity=? AND pause=0"
                    h.params))))
    ticket

let demonstrably_dead ~id ~host ~boot ~pid ~start =
  if host <> Unix.gethostname () || boot = "" || start = "" then false
  else
    let current_boot = boot_id () in
    if current_boot = "" then false
    else if current_boot <> boot then true
    else if pid = Unix.getpid () then not (Hashtbl.mem active_handlers id)
    else
      match Session_host_direct.proc_start_token pid with
      | Some current -> current <> start
      | None -> (
          try
            Unix.kill pid 0;
            false
          with
          | Unix.Unix_error (Unix.ESRCH, _, _) -> true
          | Unix.Unix_error _ -> false)

(* Called only inside verified full-gap completion's transaction. No timeout
   guesses that a running handler is dead. Unknown owners and old metadata hold. *)
let replay_abandoned db ~team ~app ~channel ~generation ~oldest ~latest =
  let params = scope db ~team ~app ~channel in
  Sql.query db
    "SELECT id,host,boot,pid,start FROM slack_order_ingress_handlers WHERE \
     installation=? AND team=? AND app=? AND channel=? AND state='active'"
    params (fun s ->
      ( Sql_util.text_column s 0,
        Sql_util.text_column s 1,
        Sql_util.text_column s 2,
        Sql_util.int_column s 3,
        Sql_util.text_column s 4 ))
  |> List.iter (fun (id, host, boot, pid, start) ->
      if demonstrably_dead ~id ~host ~boot ~pid ~start then
        Sql.exec db
          "UPDATE slack_order_ingress_handlers SET state='abandoned' WHERE \
           id=? AND state='active'"
          [ Sql.text id ]);
  Sql.exec db
    "UPDATE slack_order_ingress AS i SET applied=1 WHERE installation=? AND \
     team=? AND app=? AND channel=? AND pause=0 AND applied=0 AND CAST(ts AS \
     REAL)>=CAST(? AS REAL) AND CAST(ts AS REAL)<=CAST(? AS REAL) AND \
     EXISTS(SELECT 1 FROM slack_order_ingress_delivery d WHERE \
     d.installation=i.installation AND d.team=i.team AND d.app=i.app AND \
     d.channel=i.channel AND d.identity=i.identity AND d.gap_generation<? AND \
     d.state IN ('reserved','active','abandoned')) AND NOT EXISTS(SELECT 1 \
     FROM slack_order_ingress_handlers h WHERE h.installation=i.installation \
     AND h.team=i.team AND h.app=i.app AND h.channel=i.channel AND \
     h.identity=i.identity AND h.state NOT IN ('complete','abandoned'))"
    (params @ Sql.[ text oldest; text latest; int generation ]);
  Sql.exec db
    "UPDATE slack_order_ingress_delivery AS d SET state='replayed' WHERE \
     installation=? AND team=? AND app=? AND channel=? AND state<>'applied' \
     AND EXISTS(SELECT 1 FROM slack_order_ingress i WHERE \
     i.installation=d.installation AND i.team=d.team AND i.app=d.app AND \
     i.channel=d.channel AND i.identity=d.identity AND i.pause=0 AND \
     i.applied=1)"
    params

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
