include Slack_standing_order_recovery_base

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

let reserve_notice ?(check = fun () -> true) ?advertised db ~batch_id ~owner
    ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when check ()
             && batch_fenced db b owner now
             && Option.fold ~none:true
                  ~some:(fun t -> Float.is_finite t && t > 0.)
                  advertised ->
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             status='notice_attempted',release_at=COALESCE(?,release_at) WHERE \
             id=? AND status='pending'"
            [
              Option.fold ~none:Sqlite3.Data.NULL ~some:f advertised;
              Sql.text batch_id;
            ];
          Sqlite3.changes db = 1
      | _ -> false)

let bind_notice ?(check = fun () -> true) db ~batch_id ~owner ~ts ~release_at
    ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when check () && T.valid_slack_ts ts && Float.is_finite release_at
             && release_at >= now +. 600.
             && release_at >= Option.value b.release_at ~default:neg_infinity
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
             && covered db b.order_id b.epoch
             && phase db b.order_id = Some "complete" ->
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             status='released' WHERE id=? AND status='confirmed' AND notice_ts \
             IS NOT NULL AND release_at<=? AND EXISTS(SELECT 1 FROM \
             slack_standing_order_cards c WHERE c.id=card_id AND \
             c.message_ts=notice_ts AND c.order_epoch=epoch AND c.kind='pause' \
             AND c.order_id=slack_standing_order_recovery_batches.order_id AND \
             EXISTS(SELECT 1 FROM slack_standing_orders o WHERE \
             o.id=c.order_id AND o.team_id=c.team_id AND o.api_app_id=c.app_id \
             AND o.channel_id=c.channel_id AND o.epoch=c.order_epoch))"
            Sql.[ text batch_id; f now ];
          Sqlite3.changes db = 1
      | _ -> false)

let adopt_batch_owner ?(check = fun () -> true) db ~batch_id ~owner ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b
        when check ()
             && fenced db ~order_id:b.order_id ~owner ~now
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

let current_owner db ~order_id =
  Sql.query db
    "SELECT owner,generation,until FROM slack_standing_order_recovery_passes \
     WHERE order_id=?"
    [ Sql.text order_id ]
    (fun s ->
      {
        owner_id = Sql_util.text_column s 0;
        generation = Sql_util.int_column s 1;
        until = Option.get (Sql.opt_float_column s 2);
      })
  |> Sql.first

let batch_details db ~batch_id =
  Sql.query db
    "SELECT status,card_id FROM slack_standing_order_recovery_batches WHERE \
     id=?"
    [ Sql.text batch_id ]
    (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
  |> Sql.first

let batch_count db ~batch_id =
  Sql.int_query db
    "SELECT count(*) FROM slack_standing_order_recovery_candidates WHERE \
     batch_id=?"
    [ Sql.text batch_id ]

let notice_unknown ?(check = fun () -> true) db ~batch_id ~owner ~now =
  tx_bool db (fun () ->
      match find_batch db ~batch_id with
      | Some b when check () && batch_fenced db b owner now ->
          Sql.exec db
            "UPDATE slack_standing_order_recovery_batches SET \
             status='notice_unknown' WHERE id=? AND status='notice_attempted'"
            [ Sql.text batch_id ];
          true
      | _ -> false)

(* The caller supplies eligible fires, never an advertised count. Resolve their
   pinned identities under the same write lock that freezes batch membership. *)
let prepare_batch ~db ~(order : T.order) ~fire_ids ~now =
  match
    Sql.in_tx db ~on_error:Fun.id (fun () ->
        let owner = Option.get (current_owner db ~order_id:order.id) in
        if
          not
            (fenced db ~order_id:order.id ~owner ~now
            && ready db order.id
            && covered db order.id order.epoch
            && phase db order.id = Some "complete")
        then failwith "catch-up requires current verified recovery coverage";
        let ids = List.sort_uniq String.compare fire_ids in
        if ids = [] then invalid_arg "catch-up requires eligible fires";
        let candidates =
          List.map
            (fun id ->
              match
                Sql.query db
                  "SELECT c.root_ts,c.date_start,c.batch_id FROM \
                   slack_standing_order_recovery_candidates c JOIN \
                   slack_standing_order_fires f ON f.order_id=c.order_id AND \
                   f.root_ts=c.root_ts AND f.room_id=c.room_id AND \
                   f.date_start=c.date_start WHERE f.id=? AND c.order_id=? AND \
                   c.epoch=? AND f.epoch=c.epoch AND c.origin='recovery' AND \
                   (c.batch_id IS NOT NULL OR (f.dispatched_at IS NULL AND \
                   f.state IN ('detected','fetching','uploading') AND \
                   f.cancel_requested=0 AND f.repost_forbidden=0)) AND NOT \
                   EXISTS(SELECT 1 FROM slack_standing_order_suppression s \
                   WHERE s.order_id=c.order_id AND (s.root_ts=c.root_ts OR \
                   (s.room_id=c.room_id AND s.date_start=c.date_start)))"
                  Sql.[ text id; text order.id; int order.epoch ]
                  (fun s ->
                    ( Sql_util.text_column s 0,
                      Sql_util.text_column s 1,
                      Sql_util.opt_text_column s 2 ))
              with
              | [ row ] -> row
              | _ -> failwith "ineligible catch-up fire")
            ids
        in
        let roots =
          List.filter_map
            (fun (r, s, b) -> if b = None then Some (r, s) else None)
            candidates
        in
        match roots with
        | [] ->
            let _, _, id = List.hd candidates in
            Ok (Option.get (find_batch db ~batch_id:(Option.get id)))
        | _ ->
            let card =
              Store.insert_card db ~kind:T.Pause_button ~order
                ~created_by:order.grantor_user_id ~now ()
            in
            (* One opaque identifier names both the batch and its unique Pause card.
         The fixed Block Kit marker therefore binds both without a secret. *)
            let id = card.id in
            let gap = pass_token db order.id in
            Sql.exec db
              "INSERT INTO \
               slack_standing_order_recovery_batches(id,order_id,epoch,status,card_id,gap_generation,owner_generation) \
               VALUES(?,?,?,'pending',?,?,?)"
              Sql.
                [
                  text id;
                  text order.id;
                  int order.epoch;
                  text card.id;
                  int gap.gap_generation;
                  int owner.generation;
                ];
            List.iter
              (fun (root, start) ->
                Sql.exec db
                  "UPDATE slack_standing_order_recovery_candidates SET \
                   batch_id=? WHERE order_id=? AND root_ts=? AND date_start=? \
                   AND epoch=? AND batch_id IS NULL"
                  Sql.
                    [
                      text id;
                      text order.id;
                      text root;
                      text start;
                      int order.epoch;
                    ];
                if Sqlite3.changes db <> 1 then
                  failwith "catch-up membership changed")
              roots;
            Ok (Option.get (find_batch db ~batch_id:id)))
  with
  | Ok b -> b
  | Error e -> failwith e

(* The public show/status facade exposes notice uncertainty even if management
   reporting was refused. No instruction, huddle content or secret is included. *)
let status_json db ~order_id =
  let batches =
    try
      Sql.query db
        "SELECT id,epoch,status,notice_ts,release_at FROM \
         slack_standing_order_recovery_batches WHERE order_id=? ORDER BY rowid"
        [ Sql.text order_id ]
        (fun s ->
          `Assoc
            [
              ("id", `String (Sql_util.text_column s 0));
              ("epoch", `Int (Sql_util.int_column s 1));
              ("status", `String (Sql_util.text_column s 2));
              ( "notice_ts",
                match Sql_util.opt_text_column s 3 with
                | None -> `Null
                | Some v -> `String v );
              ( "release_at",
                match Sql.opt_float_column s 4 with
                | None -> `Null
                | Some v -> `Float v );
            ])
      |> fun xs -> `List xs
    with exn when Sql.is_db_error exn -> `Null
  in
  match Slack_standing_order_recovery_view.status_json db ~order_id with
  | `Assoc fields ->
      let origin =
        try
          Sql.query db
            "SELECT status FROM slack_order_coverage_origin WHERE order_id=?"
            [ Sql.text order_id ]
            (fun s -> `String (Sql_util.text_column s 0))
          |> Sql.first
          |> Option.value ~default:`Null
        with _ -> `Null
      in
      `Assoc
        (( "retention",
           Slack_standing_order_cleanup_store.status_json db ~order_id )
        :: ("coverage_origin", origin)
        :: ("catchup_batches", batches)
        :: fields)
  | json -> json

let yield_owner db ~order_id ~owner ~now =
  Sql.exec db
    "UPDATE slack_standing_order_recovery_passes SET until=? WHERE order_id=? \
     AND owner=? AND generation=? AND until>?"
    (f now :: owner_params order_id owner now);
  Sqlite3.changes db = 1

let resume_owner db ~order_id ~owner ~now =
  Sql.exec db
    "UPDATE slack_standing_order_recovery_passes SET until=? WHERE order_id=? \
     AND owner=? AND generation=? AND until=?"
    Sql.
      [
        f (now +. 120.);
        text order_id;
        text owner.owner_id;
        int owner.generation;
        f now;
      ];
  Sqlite3.changes db = 1

(* Only the completed verified fetch may populate retention source proof. The
   attempt source fields alone can contain pre-content hints and are insufficient. *)
let record_cleanup_sources db ~fire_id ~lease ~canvas ~transcript ~now =
  tx_bool db (fun () ->
      match Store.runtime db ~fire_id with
      | Some r
        when Store.valid_lease lease r now
             && Store.sources_mutable r && r.canvas_id = Some canvas
             && r.transcript_id = transcript ->
          Sql.exec db
            "UPDATE slack_standing_order_fires SET \
             source_canvas_id=?,source_transcript_id=? WHERE id=?"
            Sql.[ text canvas; Sqlite3.Data.opt_text transcript; text fire_id ];
          Sqlite3.changes db = 1
      | _ -> false)
