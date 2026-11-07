module Sql = Slack_standing_order_sql
(** Own-artifact maintenance has independent leases and no order-epoch fence.
    Rows are deliberately not children of the expiring huddle observation
    ledger. *)

module Store = Slack_standing_order_store
module T = Slack_standing_order_types
module Rollout = Slack_standing_order_rollout_store

let f x = Sqlite3.Data.FLOAT x
let sorted = List.sort_uniq String.compare
let get = Slack_standing_order_pages.get
let field = Slack_standing_order_pages.field
let valid_file = Slack_config_types.valid_id "F"
let opt = Sqlite3.Data.opt_text

let json_ids ids =
  Yojson.Safe.to_string (`List (List.map (fun s -> `String s) ids))

let ids raw =
  Yojson.Safe.from_string raw
  |> Yojson.Safe.Util.to_list
  |> List.map Yojson.Safe.Util.to_string

let ensure_schema db =
  Store.ensure_schema db;
  Sql_util.exec_schema ~label:"standing_order_cleanup" db
    "CREATE TABLE IF NOT EXISTS slack_order_cleanup_jobs (id TEXT PRIMARY KEY, \
     order_id TEXT NOT NULL, canvas TEXT NOT NULL, transcript TEXT, own_user \
     TEXT NOT NULL, own_bot TEXT NOT NULL, missing INTEGER NOT NULL DEFAULT 0, \
     checked_at REAL, next_at REAL NOT NULL DEFAULT 0, source_cursor INTEGER \
     NOT NULL DEFAULT 0, owner TEXT, generation INTEGER NOT NULL DEFAULT 0, \
     until REAL NOT NULL DEFAULT 0, status TEXT NOT NULL DEFAULT 'watching'); \
     CREATE TABLE IF NOT EXISTS slack_order_cleanup_files (job TEXT NOT NULL, \
     file TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'pending', PRIMARY \
     KEY(job,file)); CREATE TABLE IF NOT EXISTS slack_order_cleanup_notes (job \
     TEXT NOT NULL, thread TEXT NOT NULL, status TEXT NOT NULL DEFAULT \
     'pending', ts TEXT, cursor TEXT NOT NULL DEFAULT '', seen TEXT NOT NULL \
     DEFAULT '[]', match_ts TEXT, latest TEXT, ambiguous INTEGER NOT NULL \
     DEFAULT 0, pages INTEGER NOT NULL DEFAULT 0, PRIMARY KEY(job,thread));"

type job = {
  id : string;
  order_id : string;
  canvas : string;
  transcript : string option;
  own_user : string;
  own_bot : string;
  missing : bool;
  checked_at : float option;
  source_cursor : int;
  status : string;
}

type owner = { id : string; generation : int }

let find db id =
  Sql.query db
    "SELECT \
     id,order_id,canvas,transcript,own_user,own_bot,missing,checked_at,source_cursor,status \
     FROM slack_order_cleanup_jobs WHERE id=?"
    [ Sql.text id ]
    (fun s ->
      {
        id = Sql_util.text_column s 0;
        order_id = Sql_util.text_column s 1;
        canvas = Sql_util.text_column s 2;
        transcript = Sql_util.opt_text_column s 3;
        own_user = Sql_util.text_column s 4;
        own_bot = Sql_util.text_column s 5;
        missing = Sql_util.sql_bool s 6;
        checked_at = Sql.opt_float_column s 7;
        source_cursor = Sql_util.int_column s 8;
        status = Sql_util.text_column s 9;
      })
  |> Sql.first

let files db id =
  Sql.query db
    "SELECT file,status FROM slack_order_cleanup_files WHERE job=? ORDER BY \
     file"
    [ Sql.text id ]
    (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))

let threads db id =
  Sql.query db
    "SELECT thread,status FROM slack_order_cleanup_notes WHERE job=? ORDER BY \
     CAST(thread AS REAL)"
    [ Sql.text id ]
    (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))

let sources (j : job) =
  j.canvas :: List.filter (( <> ) j.canvas) (Option.to_list j.transcript)

let recent now t =
  Float.is_finite t && t > 0. && t <= now && t >= now -. 2592000.

let stamp ts =
  if Slack_standing_order_pages.valid_ts ts then float_of_string_opt ts
  else None

let install_job db (o : T.order) ~id ~canvas ~transcript ~own_user ~own_bot
    ~files:own_files ~threads:relays =
  let sources = canvas :: Option.to_list transcript in
  if
    valid_file canvas
    && Option.fold ~none:true ~some:valid_file transcript
    && own_files <> []
    && List.for_all valid_file own_files
    && List.for_all (fun id -> not (List.mem id sources)) own_files
    && Some own_user = o.bot_user_id
    && Some own_bot = o.bot_id
  then (
    Sql.exec db
      "INSERT OR IGNORE INTO \
       slack_order_cleanup_jobs(id,order_id,canvas,transcript,own_user,own_bot) \
       VALUES(?,?,?,?,?,?)"
      Sql.
        [
          text id;
          text o.id;
          text canvas;
          opt transcript;
          text own_user;
          text own_bot;
        ];
    match find db id with
    | Some j
      when j.order_id = o.id && j.canvas = canvas && j.transcript = transcript
           && j.own_user = own_user && j.own_bot = own_bot ->
        let changed = ref false in
        List.iter
          (fun file ->
            Sql.exec db
              "INSERT OR IGNORE INTO slack_order_cleanup_files(job,file) \
               VALUES(?,?)"
              Sql.[ text id; text file ];
            if Sqlite3.changes db = 1 then changed := true)
          own_files;
        List.iter
          (fun thread ->
            Sql.exec db
              "INSERT OR IGNORE INTO slack_order_cleanup_notes(job,thread) \
               VALUES(?,?)"
              Sql.[ text id; text thread ];
            if Sqlite3.changes db = 1 then changed := true)
          relays;
        if !changed then
          Sql.exec db
            "UPDATE slack_order_cleanup_jobs SET status=CASE WHEN missing=1 \
             THEN 'cleaning' ELSE 'watching' END WHERE id=?"
            [ Sql.text id ]
    | _ -> ())

let transaction db work =
  match Sql.in_tx db ~on_error:Fun.id (fun () -> Ok (work ())) with
  | Ok x -> x
  | Error e -> failwith e

let refresh db (o : T.order) ~now =
  ensure_schema db;
  transaction db (fun () ->
      let recent_ids = ref [] in
      let remember id = recent_ids := id :: !recent_ids in
      Sql.query db
        "SELECT id,source_canvas_id,source_transcript_id FROM \
         slack_standing_order_fires WHERE order_id=?"
        [ Sql.text o.id ]
        (fun s ->
          ( Sql_util.text_column s 0,
            Sql_util.opt_text_column s 1,
            Sql_util.opt_text_column s 2 ))
      |> List.iter (fun (id, canvas, transcript) ->
          let attempts =
            Sql.query db
              ("SELECT " ^ Store.attempt_columns
             ^ " FROM slack_standing_order_dispatches WHERE fire_id=? ORDER BY \
                attempt_no")
              [ Sql.text id ]
              Store.decode_attempt
          in
          let dispatched =
            List.filter
              (fun (a : Store.relay_attempt) ->
                a.attempt_no >= 0 && a.attempt_no <= 1
                && (match a.dispatched_at with
                  | Some t -> Float.is_finite t && t > 0. && t <= now
                  | None -> false)
                && Option.fold ~none:true
                     ~some:(fun ts ->
                       Option.fold ~none:false
                         ~some:(fun t ->
                           Float.is_finite t && t > 0. && t <= now)
                         (stamp ts))
                     a.relay_ts)
              attempts
          in
          if
            Option.fold ~none:false
              ~some:(fun (j : job) -> j.missing)
              (find db id)
            || List.exists
                 (fun (a : Store.relay_attempt) ->
                   recent now
                     (Option.value
                        (Option.bind a.relay_ts stamp)
                        ~default:(Option.get a.dispatched_at)))
                 dispatched
          then
            match (canvas, o.bot_user_id, o.bot_id) with
            | Some canvas, Some own_user, Some own_bot ->
                remember id;
                install_job db o ~id ~canvas ~transcript ~own_user ~own_bot
                  ~files:
                    (List.concat_map
                       (fun (a : Store.relay_attempt) -> a.uploaded_file_ids)
                       dispatched
                    |> sorted)
                  ~threads:
                    (List.filter_map
                       (fun (a : Store.relay_attempt) -> a.relay_ts)
                       dispatched
                    |> sorted)
            | _ -> ());
      (* Imported rows are append-only reviewed proof. Revalidate their exact digest,
       scope and own identity, never interpret editable relay prose as proof. *)
      Sql.query db
        "SELECT e.receipt_digest,e.metadata,e.automatic_fire FROM \
         slack_standing_order_external_deliveries e JOIN \
         slack_standing_order_suppression s ON \
         s.receipt_digest=e.receipt_digest WHERE s.order_id=?"
        [ Sql.text o.id ]
        (fun s ->
          ( Sql_util.text_column s 0,
            Sql_util.text_column s 1,
            Sql_util.opt_text_column s 2 ))
      |> List.iter (fun (digest, raw, automatic) ->
          let r =
            Slack_standing_order_receipt_import.receipt_of_json
              (Yojson.Safe.from_string raw)
          in
          let _, installation, _, _ = Rollout.read_anchor db in
          if
            r.evidence_digest = digest
            && Slack_standing_order_receipt_import.receipt_digest r = digest
            && r.installation = installation
            && r.order_id = o.id
            && r.channel_id = o.channel_id
            && r.identity.team_id = o.team_id
            && r.identity.app_id = o.api_app_id
            && r.identity.enterprise_id = o.enterprise_id
            && Option.fold ~none:false ~some:(recent now) (stamp r.relay_ts)
          then
            match r.source_canvas_id with
            | Some canvas ->
                let id =
                  Option.value automatic ~default:("external:" ^ digest)
                in
                remember id;
                install_job db o ~id ~canvas ~transcript:r.source_transcript_id
                  ~own_user:r.identity.own_user_id
                  ~own_bot:r.identity.own_bot_id ~files:r.own_file_ids
                  ~threads:[ r.relay_ts ]
            | None -> ());
      Sql.query db
        "SELECT id FROM slack_order_cleanup_jobs WHERE order_id=? AND \
         missing=0 AND status='watching'"
        [ Sql.text o.id ]
        (fun s -> Sql_util.text_column s 0)
      |> List.iter (fun id ->
          if not (List.mem id !recent_ids) then
            Sql.exec db
              "UPDATE slack_order_cleanup_jobs SET status='expired' WHERE id=?"
              [ Sql.text id ]))

let due db ~order_id ~now =
  Sql.query db
    "SELECT id FROM slack_order_cleanup_jobs WHERE order_id=? AND status IN \
     ('watching','cleaning') AND next_at<=? AND until<=? AND (missing=1 OR \
     checked_at IS NULL OR checked_at<=? OR source_cursor>0) ORDER BY \
     next_at,id LIMIT 20"
    Sql.[ text order_id; f now; f now; f (now -. 300.) ]
    (fun s -> Sql_util.text_column s 0)

let claim db ~id ~now =
  transaction db (fun () ->
      let owner = Store.new_id ~prefix:"cleanup" in
      Sql.exec db
        "UPDATE slack_order_cleanup_jobs SET \
         owner=?,generation=generation+1,until=? WHERE id=? AND until<=? AND \
         status IN ('watching','cleaning') AND next_at<=?"
        Sql.[ text owner; f (now +. 120.); text id; f now; f now ];
      if Sqlite3.changes db <> 1 then None
      else
        Sql.query db
          "SELECT generation FROM slack_order_cleanup_jobs WHERE id=?"
          [ Sql.text id ]
          (fun s -> { id = owner; generation = Sql_util.int_column s 0 })
        |> Sql.first)

let owns db ~job ~owner ~now =
  Sql.int_query db
    "SELECT count(*) FROM slack_order_cleanup_jobs WHERE id=? AND owner=? AND \
     generation=? AND until>?"
    Sql.[ text job; text owner.id; int owner.generation; f now ]
  = 1

let update db ~job ~owner ~now work =
  transaction db (fun () ->
      if owns db ~job ~owner ~now then (
        work ();
        true)
      else false)

let renew db ~job ~owner ~now =
  update db ~job ~owner ~now (fun () ->
      Sql.exec db "UPDATE slack_order_cleanup_jobs SET until=? WHERE id=?"
        Sql.[ f (now +. 120.); text job ])

let release db ~job ~owner ~now =
  ignore
    (update db ~job ~owner ~now (fun () ->
         Sql.exec db
           "UPDATE slack_order_cleanup_jobs SET until=0,owner=NULL WHERE id=?"
           [ Sql.text job ]))

let defer db ~job ~owner ~now =
  ignore
    (update db ~job ~owner ~now (fun () ->
         Sql.exec db "UPDATE slack_order_cleanup_jobs SET next_at=? WHERE id=?"
           Sql.[ f (now +. 300.); text job ]))

let reserve_source db ~job ~owner ~now index =
  update db ~job ~owner ~now (fun () ->
      Sql.exec db
        "UPDATE slack_order_cleanup_jobs SET \
         checked_at=?,source_cursor=?,next_at=? WHERE id=?"
        Sql.[ f now; int (index + 1); f (now +. 300.); text job ])

let source_result db ~job ~owner ~now ~missing ~finished =
  ignore
    (update db ~job ~owner ~now (fun () ->
         Sql.exec db
           "UPDATE slack_order_cleanup_jobs SET \
            missing=MAX(missing,?),source_cursor=CASE WHEN ? THEN 0 ELSE \
            source_cursor END,status=CASE WHEN missing=1 OR ? THEN 'cleaning' \
            ELSE status END WHERE id=?"
           Sql.[ bool missing; bool finished; bool missing; text job ]))

let file_status db ~job ~owner ~now file status =
  update db ~job ~owner ~now (fun () ->
      Sql.exec db
        "UPDATE slack_order_cleanup_files SET status=? WHERE job=? AND file=? \
         AND status<>'removed'"
        Sql.[ text status; text job; text file ])

let finish db ~job ~owner ~now =
  ignore
    (update db ~job ~owner ~now (fun () ->
         Sql.exec db
           "UPDATE slack_order_cleanup_jobs SET status='complete' WHERE id=? \
            AND missing=1 AND EXISTS(SELECT 1 FROM slack_order_cleanup_notes \
            WHERE job=?) AND NOT EXISTS(SELECT 1 FROM \
            slack_order_cleanup_files WHERE job=? AND status<>'removed') AND \
            NOT EXISTS(SELECT 1 FROM slack_order_cleanup_notes WHERE job=? AND \
            status<>'confirmed')"
           Sql.[ text job; text job; text job; text job ]))

let status_json db ~order_id =
  if not (Rollout.table_exists db "slack_order_cleanup_jobs") then `List []
  else
    `List
      (Sql.query db
         "SELECT id,status,missing,checked_at FROM slack_order_cleanup_jobs \
          WHERE order_id=? ORDER BY id"
         [ Sql.text order_id ]
         (fun s ->
           let id = Sql_util.text_column s 0 in
           let states rows =
             `List
               (List.map
                  (fun (id, status) ->
                    `Assoc [ ("id", `String id); ("status", `String status) ])
                  rows)
           in
           `Assoc
             [
               ("id", `String id);
               ("files", states (files db id));
               ("notes", states (threads db id));
               ("status", `String (Sql_util.text_column s 1));
               ("source_missing", `Bool (Sql_util.sql_bool s 2));
               ( "checked_at",
                 Option.fold ~none:`Null
                   ~some:(fun x -> `Float x)
                   (Sql.opt_float_column s 3) );
             ]))
