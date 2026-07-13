(* Full-runtime bridge from a verified GitHub App callback to a durable,
   confirmable setup plan. This module deliberately has no apply capability. *)

type delivery = {
  plan_id : string;
  target : string;
  room_id : string option;
  session_key : string option;
  message : string;
  created_at : string;
}

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_setup_resume_delivery (
      plan_id TEXT PRIMARY KEY NOT NULL,
      target TEXT NOT NULL,
      room_id TEXT,
      session_key TEXT,
      message TEXT NOT NULL,
      created_at TEXT NOT NULL
    )|}
  in
  let index_sql =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_setup_resume_delivery_target
      ON github_app_setup_resume_delivery(room_id, session_key, created_at)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github app setup runtime schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; index_sql ]

let room_is_active ~(config : Runtime_config.t) ~room_id =
  List.exists
    (fun (binding : Runtime_config.room_profile_binding) ->
      binding.active && binding.room = room_id)
    config.room_profile_bindings

let target_fields (result : Github_app_setup_resume.resume_result) =
  match result.target with
  | Github_app_setup_resume.Active_room room_id ->
      ( "active_room",
        Some room_id,
        None,
        Printf.sprintf
          "GitHub App setup is ready to confirm in Room %s. Plan: %s (digest \
           %s). Callback verification did not apply it."
          room_id result.plan.id result.plan.digest )
  | Github_app_setup_resume.Notification { room_id; session_key; message } ->
      ("notification", room_id, session_key, message)

let persist_delivery ~db (result : Github_app_setup_resume.resume_result) =
  ensure_schema db;
  let target, room_id, session_key, message = target_fields result in
  let created_at = Time_util.iso8601_utc () in
  let sql =
    {|INSERT OR REPLACE INTO github_app_setup_resume_delivery
        (plan_id, target, room_id, session_key, message, created_at)
      VALUES (?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i value = ignore (Sqlite3.bind stmt i value) in
  bind 1 (Sqlite3.Data.TEXT result.plan.id);
  bind 2 (Sqlite3.Data.TEXT target);
  bind 3
    (match room_id with
    | Some value -> Sqlite3.Data.TEXT value
    | None -> Sqlite3.Data.NULL);
  bind 4
    (match session_key with
    | Some value -> Sqlite3.Data.TEXT value
    | None -> Sqlite3.Data.NULL);
  bind 5 (Sqlite3.Data.TEXT message);
  bind 6 (Sqlite3.Data.TEXT created_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      Ok
        {
          plan_id = result.plan.id;
          target;
          room_id;
          session_key;
          message;
          created_at;
        }
  | rc ->
      Error
        (Printf.sprintf "failed to persist GitHub App setup resume delivery: %s"
           (Sqlite3.Rc.to_string rc))

let resume_verified_exchange ~db ~(config : Runtime_config.t)
    (exchange : Github_app_setup_callback.exchange_result) =
  let room_active =
    match exchange.transaction.bind with
    | Github_app_setup_tx.Room room_id -> room_is_active ~config ~room_id
    | Github_app_setup_tx.Session _ -> false
  in
  match
    Github_app_setup_resume.resume_after_exchange ~db ~exchange
      ~installation:(Some exchange.verified_installation) ~room_active
      ~current_base_revision:(Setup_plan.base_revision_of_config config)
      ()
  with
  | Error error ->
      ignore
        (Github_route_ops.record_audit ~db
           ~installation_id:exchange.verified_installation.installation_id
           ~action:"callback_resume_failed"
           ~details:(`Assoc [ ("error", `String error) ])
           ());
      Error error
  | Ok result -> (
      match persist_delivery ~db result with
      | Error error ->
          ignore
            (Github_route_ops.record_audit ~db ~setup_plan_id:result.plan.id
               ~installation_id:exchange.verified_installation.installation_id
               ~action:"callback_resume_delivery_failed"
               ~details:(`Assoc [ ("error", `String error) ])
               ());
          Error error
      | Ok _delivery ->
          ignore
            (Github_route_ops.record_audit ~db ~setup_plan_id:result.plan.id
               ~installation_id:exchange.verified_installation.installation_id
               ~action:"callback_resumed_confirmable_plan"
               ~details:
                 (`Assoc
                    [
                      ("exchange_receipt_id", `String exchange.receipt_id);
                      ( "target",
                        `String
                          (match result.target with
                          | Github_app_setup_resume.Active_room _ ->
                              "active_room"
                          | Github_app_setup_resume.Notification _ ->
                              "notification") );
                      ("apply", `Bool false);
                    ])
               ());
          Ok ())

let install_callback_resume ~db ~current_config =
  Github_app_setup_callback.set_resume_hook (fun exchange ->
      resume_verified_exchange ~db ~config:(current_config ()) exchange)

let text_column stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT value -> value | _ -> ""

let option_text_column stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT value -> Some value
  | _ -> None

let list_deliveries ~db ?room_id ?session_key ?(limit = 100) () =
  ensure_schema db;
  let limit = max 1 (min 1000 limit) in
  let where, bindings =
    match (room_id, session_key) with
    | Some room_id, _ -> (" WHERE room_id = ?", [ room_id ])
    | None, Some session_key -> (" WHERE session_key = ?", [ session_key ])
    | None, None -> ("", [])
  in
  let stmt =
    Sqlite3.prepare db
      ("SELECT plan_id, target, room_id, session_key, message, created_at FROM \
        github_app_setup_resume_delivery" ^ where
     ^ " ORDER BY created_at DESC LIMIT ?")
  in
  List.iteri
    (fun index value ->
      ignore (Sqlite3.bind stmt (index + 1) (Sqlite3.Data.TEXT value)))
    bindings;
  ignore
    (Sqlite3.bind stmt
       (List.length bindings + 1)
       (Sqlite3.Data.INT (Int64.of_int limit)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    rows :=
      {
        plan_id = text_column stmt 0;
        target = text_column stmt 1;
        room_id = option_text_column stmt 2;
        session_key = option_text_column stmt 3;
        message = text_column stmt 4;
        created_at = text_column stmt 5;
      }
      :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows
