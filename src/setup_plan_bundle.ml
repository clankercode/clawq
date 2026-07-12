(* Setup-owned managed access-bundle linkages. *)

type provenance = {
  setup_plan_id : string option;
  owner : string;
  feature_id : string;
  created_at : string;
}

type linkage = {
  id : string;
  room_id : string;
  bundle_id : string;
  provenance : provenance;
  status : string;
  attached_at : string;
  detached_at : string option;
}

type attach_result =
  | Attached of { linkage : linkage; first_time : bool }
  | Reused of { linkage : linkage }

type detach_result =
  | Detached of { linkage : linkage }
  | Still_attached of { linkage : linkage; remaining_features : int }
  | Not_found
  | Preserved_manual

let owner_setup = "setup"

let init_schema db =
  Sqlite3.busy_timeout db 5_000;
  let sql =
    {|CREATE TABLE IF NOT EXISTS setup_owned_bundle_links (
      id TEXT PRIMARY KEY NOT NULL,
      room_id TEXT NOT NULL,
      bundle_id TEXT NOT NULL,
      feature_id TEXT NOT NULL,
      setup_plan_id TEXT,
      owner TEXT NOT NULL DEFAULT 'setup',
      status TEXT NOT NULL DEFAULT 'attached',
      attached_at TEXT NOT NULL,
      detached_at TEXT,
      UNIQUE(room_id, bundle_id, feature_id)
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_setup_owned_bundle_room
      ON setup_owned_bundle_links(room_id, status)|}
  in
  List.iter
    (fun s ->
      match Sqlite3.exec db s with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "setup_plan_bundle schema: %s"
               (Sqlite3.Rc.to_string rc)))
    [ sql; idx ]

let row_of_stmt stmt : linkage =
  let text i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let opt_text i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None
  in
  {
    id = text 0;
    room_id = text 1;
    bundle_id = text 2;
    provenance =
      {
        setup_plan_id = opt_text 4;
        owner = text 5;
        feature_id = text 3;
        created_at = text 7;
      };
    status = text 6;
    attached_at = text 7;
    detached_at = opt_text 8;
  }

let find_link ~db ~room_id ~bundle_id ~feature_id =
  let sql =
    {|SELECT id, room_id, bundle_id, feature_id, setup_plan_id, owner, status,
             attached_at, detached_at
      FROM setup_owned_bundle_links
      WHERE room_id = ? AND bundle_id = ? AND feature_id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT bundle_id));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT feature_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Some (row_of_stmt stmt)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let count_attached_features ~db ~room_id ~bundle_id () =
  let sql =
    {|SELECT COUNT(*) FROM setup_owned_bundle_links
      WHERE room_id = ? AND bundle_id = ? AND status = 'attached'
        AND owner = 'setup'|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT bundle_id));
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  n

let is_setup_owned ~db ~room_id ~bundle_id () =
  count_attached_features ~db ~room_id ~bundle_id () > 0

let attach ~db ~room_id ~bundle_id ~feature_id ?setup_plan_id
    ?(now = Unix.gettimeofday ()) () =
  match find_link ~db ~room_id ~bundle_id ~feature_id with
  | Some link
    when link.status = "attached" && link.provenance.owner = owner_setup ->
      Ok (Reused { linkage = link })
  | Some link when link.provenance.owner <> owner_setup ->
      Error "bundle linkage is not setup-owned; manual grants are preserved"
  | Some link ->
      (* Re-attach a previously detached setup-owned feature. *)
      let attached_at = Time_util.iso8601_utc ~t:now () in
      let sql =
        {|UPDATE setup_owned_bundle_links
          SET status = 'attached', attached_at = ?, detached_at = NULL,
              setup_plan_id = COALESCE(?, setup_plan_id)
          WHERE id = ?|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT attached_at));
      ignore
        (Sqlite3.bind stmt 2
           (match setup_plan_id with
           | None -> Sqlite3.Data.NULL
           | Some p -> Sqlite3.Data.TEXT p));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT link.id));
      let rc = Sqlite3.step stmt in
      ignore (Sqlite3.finalize stmt);
      if rc <> Sqlite3.Rc.DONE then Error "failed to re-attach linkage"
      else
        let updated =
          {
            link with
            status = "attached";
            attached_at;
            detached_at = None;
            provenance =
              {
                link.provenance with
                setup_plan_id =
                  (match setup_plan_id with
                  | Some p -> Some p
                  | None -> link.provenance.setup_plan_id);
                created_at = attached_at;
              };
          }
        in
        Ok (Attached { linkage = updated; first_time = false })
  | None -> (
      let id =
        Printf.sprintf "sblink_%d_%06d" (int_of_float now)
          (Random.int 1_000_000)
      in
      let attached_at = Time_util.iso8601_utc ~t:now () in
      let sql =
        {|INSERT INTO setup_owned_bundle_links
          (id, room_id, bundle_id, feature_id, setup_plan_id, owner, status,
           attached_at, detached_at)
          VALUES (?, ?, ?, ?, ?, 'setup', 'attached', ?, NULL)|}
      in
      let stmt = Sqlite3.prepare db sql in
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT id);
      bind 2 (Sqlite3.Data.TEXT room_id);
      bind 3 (Sqlite3.Data.TEXT bundle_id);
      bind 4 (Sqlite3.Data.TEXT feature_id);
      bind 5
        (match setup_plan_id with
        | None -> Sqlite3.Data.NULL
        | Some p -> Sqlite3.Data.TEXT p);
      bind 6 (Sqlite3.Data.TEXT attached_at);
      let rc = Sqlite3.step stmt in
      ignore (Sqlite3.finalize stmt);
      match rc with
      | Sqlite3.Rc.DONE ->
          let linkage =
            {
              id;
              room_id;
              bundle_id;
              provenance =
                {
                  setup_plan_id;
                  owner = owner_setup;
                  feature_id;
                  created_at = attached_at;
                };
              status = "attached";
              attached_at;
              detached_at = None;
            }
          in
          Ok (Attached { linkage; first_time = true })
      | Sqlite3.Rc.CONSTRAINT -> (
          (* Concurrent insert of same unique key — treat as reuse. *)
          match find_link ~db ~room_id ~bundle_id ~feature_id with
          | Some link -> Ok (Reused { linkage = link })
          | None -> Error "attach constraint failed")
      | rc ->
          Error (Printf.sprintf "attach failed: %s" (Sqlite3.Rc.to_string rc)))

let record_managed_feature ~db ~room_id ~bundle_id ~feature_id () =
  match attach ~db ~room_id ~bundle_id ~feature_id () with
  | Ok _ -> Ok ()
  | Error e -> Error e

let with_remove_savepoint db f =
  match Sqlite3.exec db "SAVEPOINT setup_bundle_remove" with
  | Sqlite3.Rc.OK -> (
      match f () with
      | Ok _ as result -> (
          match Sqlite3.exec db "RELEASE SAVEPOINT setup_bundle_remove" with
          | Sqlite3.Rc.OK -> result
          | rc ->
              ignore
                (Sqlite3.exec db "ROLLBACK TO SAVEPOINT setup_bundle_remove");
              ignore (Sqlite3.exec db "RELEASE SAVEPOINT setup_bundle_remove");
              Error
                (Printf.sprintf "failed to commit managed bundle removal: %s"
                   (Sqlite3.Rc.to_string rc)))
      | Error _ as result ->
          ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT setup_bundle_remove");
          ignore (Sqlite3.exec db "RELEASE SAVEPOINT setup_bundle_remove");
          result)
  | rc ->
      Error
        (Printf.sprintf "failed to lock managed bundle removal: %s"
           (Sqlite3.Rc.to_string rc))

let remove_managed_feature ~db ~room_id ~bundle_id ~feature_id
    ?(now = Unix.gettimeofday ()) () =
  match find_link ~db ~room_id ~bundle_id ~feature_id with
  | None -> Ok Not_found
  | Some link when link.provenance.owner <> owner_setup -> Ok Preserved_manual
  | Some link when link.status <> "attached" -> Ok Not_found
  | Some link -> (
      with_remove_savepoint db @@ fun () ->
      let detached_at = Time_util.iso8601_utc ~t:now () in
      let sql =
        {|UPDATE setup_owned_bundle_links
          SET status = 'detached', detached_at = ?
          WHERE id = ? AND status = 'attached' AND owner = 'setup'|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT detached_at));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT link.id));
      let rc = Sqlite3.step stmt in
      ignore (Sqlite3.finalize stmt);
      match rc with
      | Sqlite3.Rc.DONE when Sqlite3.changes db = 0 -> Ok Not_found
      | Sqlite3.Rc.DONE ->
          let updated =
            { link with status = "detached"; detached_at = Some detached_at }
          in
          (* The savepoint holds the write lock from the guarded update through
             this count, so exactly one concurrent remover observes zero. *)
          let remaining = count_attached_features ~db ~room_id ~bundle_id () in
          if remaining > 0 then
            Ok
              (Still_attached
                 { linkage = updated; remaining_features = remaining })
          else Ok (Detached { linkage = updated })
      | rc ->
          Error
            (Printf.sprintf "failed to detach managed bundle feature: %s"
               (Sqlite3.Rc.to_string rc)))

let list_by ~db ~room_id ~status_filter () =
  let sql, bind_status =
    match status_filter with
    | None ->
        ( {|SELECT id, room_id, bundle_id, feature_id, setup_plan_id, owner, status,
                   attached_at, detached_at
            FROM setup_owned_bundle_links
            WHERE room_id = ?
            ORDER BY attached_at|},
          false )
    | Some st ->
        ( {|SELECT id, room_id, bundle_id, feature_id, setup_plan_id, owner, status,
                   attached_at, detached_at
            FROM setup_owned_bundle_links
            WHERE room_id = ? AND status = ?
            ORDER BY attached_at|},
          true )
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  (if bind_status then
     match status_filter with
     | Some st -> ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT st))
     | None -> ());
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    rows := row_of_stmt stmt :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

let inspect_room ~db ~room_id () = list_by ~db ~room_id ~status_filter:None ()

let list_attached ~db ~room_id () =
  list_by ~db ~room_id ~status_filter:(Some "attached") ()
