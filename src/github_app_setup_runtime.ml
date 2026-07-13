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

type retry = {
  receipt_id : string;
  tx_id : string;
  installation_id : int;
  plan_id : string option;
  target : string option;
  room_id : string option;
  session_key : string option;
  message : string option;
  attempts : int;
  last_error : string;
  created_at : string;
  updated_at : string;
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
  let retry_table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_setup_resume_retry (
      receipt_id TEXT PRIMARY KEY NOT NULL,
      tx_id TEXT NOT NULL,
      installation_id INTEGER NOT NULL,
      plan_id TEXT,
      target TEXT,
      room_id TEXT,
      session_key TEXT,
      message TEXT,
      attempts INTEGER NOT NULL DEFAULT 0,
      last_error TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let retry_index_sql =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_setup_resume_retry_updated
      ON github_app_setup_resume_retry(updated_at)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github app setup runtime schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; index_sql; retry_table_sql; retry_index_sql ]

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

let persist_delivery_record ~db (delivery : delivery) =
  ensure_schema db;
  let sql =
    {|INSERT OR REPLACE INTO github_app_setup_resume_delivery
        (plan_id, target, room_id, session_key, message, created_at)
      VALUES (?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i value = ignore (Sqlite3.bind stmt i value) in
  bind 1 (Sqlite3.Data.TEXT delivery.plan_id);
  bind 2 (Sqlite3.Data.TEXT delivery.target);
  bind 3
    (match delivery.room_id with
    | Some value -> Sqlite3.Data.TEXT value
    | None -> Sqlite3.Data.NULL);
  bind 4
    (match delivery.session_key with
    | Some value -> Sqlite3.Data.TEXT value
    | None -> Sqlite3.Data.NULL);
  bind 5 (Sqlite3.Data.TEXT delivery.message);
  bind 6 (Sqlite3.Data.TEXT delivery.created_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok delivery
  | rc ->
      Error
        (Printf.sprintf "failed to persist GitHub App setup resume delivery: %s"
           (Sqlite3.Rc.to_string rc))

let delivery_of_result (result : Github_app_setup_resume.resume_result) =
  let target, room_id, session_key, message = target_fields result in
  {
    plan_id = result.plan.id;
    target;
    room_id;
    session_key;
    message;
    created_at = Time_util.iso8601_utc ();
  }

let persist_delivery ~db result =
  persist_delivery_record ~db (delivery_of_result result)

let delivery_of_plan ~(config : Runtime_config.t) (plan : Setup_plan.t) =
  match (plan.destination.room_id, plan.destination.session_key) with
  | Some room_id, None ->
      let target =
        if room_is_active ~config ~room_id then "active_room"
        else "notification"
      in
      Ok
        {
          plan_id = plan.id;
          target;
          room_id = Some room_id;
          session_key = None;
          message =
            Printf.sprintf
              "GitHub App setup replacement plan is ready to confirm in Room \
               %s. Plan: %s (digest %s). It was not applied automatically."
              room_id plan.id plan.digest;
          created_at = Time_util.iso8601_utc ();
        }
  | None, Some session_key ->
      Ok
        {
          plan_id = plan.id;
          target = "notification";
          room_id = None;
          session_key = Some session_key;
          message =
            Printf.sprintf
              "GitHub App setup replacement plan is ready to confirm. Plan: %s \
               (digest %s). It was not applied automatically."
              plan.id plan.digest;
          created_at = Time_util.iso8601_utc ();
        }
  | _ -> Error "GitHub App setup replacement plan has no single destination"

let persist_replacement_delivery ~db ~(config : Runtime_config.t) ~plan =
  match delivery_of_plan ~config plan with
  | Error _ as error -> error
  | Ok delivery -> persist_delivery_record ~db delivery

let nullable_text = function
  | Some value -> Sqlite3.Data.TEXT value
  | None -> Sqlite3.Data.NULL

let text_column stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT value -> value | _ -> ""

let option_text_column stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT value -> Some value
  | _ -> None

let ensure_retry ~db (exchange : Github_app_setup_callback.exchange_result) =
  ensure_schema db;
  let now = Time_util.iso8601_utc () in
  let sql =
    {|INSERT INTO github_app_setup_resume_retry
        (receipt_id, tx_id, installation_id, attempts, last_error, created_at,
         updated_at)
      VALUES (?, ?, ?, 0, '', ?, ?)
      ON CONFLICT(receipt_id) DO NOTHING|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i value = ignore (Sqlite3.bind stmt i value) in
  bind 1 (Sqlite3.Data.TEXT exchange.receipt_id);
  bind 2 (Sqlite3.Data.TEXT exchange.transaction.id);
  bind 3
    (Sqlite3.Data.INT
       (Int64.of_int exchange.verified_installation.installation_id));
  bind 4 (Sqlite3.Data.TEXT now);
  bind 5 (Sqlite3.Data.TEXT now);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "failed to persist GitHub App callback retry record: %s"
           (Sqlite3.Rc.to_string rc))

let update_retry ~db ~(receipt_id : string) ?delivery ~error () =
  ensure_schema db;
  let now = Time_util.iso8601_utc () in
  let sql =
    {|UPDATE github_app_setup_resume_retry
      SET plan_id = COALESCE(?, plan_id), target = COALESCE(?, target),
          room_id = COALESCE(?, room_id),
          session_key = COALESCE(?, session_key),
          message = COALESCE(?, message),
          attempts = attempts + 1, last_error = ?, updated_at = ?
      WHERE receipt_id = ?|}
  in
  let plan_id, target, room_id, session_key, message =
    match delivery with
    | None -> (None, None, None, None, None)
    | Some (delivery : delivery) ->
        ( Some delivery.plan_id,
          Some delivery.target,
          delivery.room_id,
          delivery.session_key,
          Some delivery.message )
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i value = ignore (Sqlite3.bind stmt i value) in
  bind 1 (nullable_text plan_id);
  bind 2 (nullable_text target);
  bind 3 (nullable_text room_id);
  bind 4 (nullable_text session_key);
  bind 5 (nullable_text message);
  bind 6 (Sqlite3.Data.TEXT error);
  bind 7 (Sqlite3.Data.TEXT now);
  bind 8 (Sqlite3.Data.TEXT receipt_id);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "failed to update GitHub App callback retry record: %s"
           (Sqlite3.Rc.to_string rc))

let clear_retry ~db ~receipt_id =
  let stmt =
    Sqlite3.prepare db
      "DELETE FROM github_app_setup_resume_retry WHERE receipt_id = ?"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT receipt_id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "failed to clear GitHub App callback retry record: %s"
           (Sqlite3.Rc.to_string rc))

let retry_of_stmt stmt =
  let int_column i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.INT value -> Int64.to_int value
    | Sqlite3.Data.TEXT value -> ( try int_of_string value with _ -> 0)
    | _ -> 0
  in
  {
    receipt_id = text_column stmt 0;
    tx_id = text_column stmt 1;
    installation_id = int_column 2;
    plan_id = option_text_column stmt 3;
    target = option_text_column stmt 4;
    room_id = option_text_column stmt 5;
    session_key = option_text_column stmt 6;
    message = option_text_column stmt 7;
    attempts = int_column 8;
    last_error = text_column stmt 9;
    created_at = text_column stmt 10;
    updated_at = text_column stmt 11;
  }

let list_retries ~db ?(limit = 100) () =
  ensure_schema db;
  let limit = max 1 (min 1000 limit) in
  let stmt =
    Sqlite3.prepare db
      {|SELECT receipt_id, tx_id, installation_id, plan_id, target, room_id,
               session_key, message, attempts, last_error, created_at,
               updated_at
        FROM github_app_setup_resume_retry
        ORDER BY updated_at ASC LIMIT ?|}
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    rows := retry_of_stmt stmt :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

let audit_retry_failure ~db
    ~(exchange : Github_app_setup_callback.exchange_result) ?plan_id ~action
    ~error () =
  ignore
    (Github_route_ops.record_audit ~db ?setup_plan_id:plan_id
       ~installation_id:exchange.verified_installation.installation_id ~action
       ~details:
         (`Assoc
            [
              ("exchange_receipt_id", `String exchange.receipt_id);
              ("error", `String error);
              ("retry", `Bool true);
            ])
       ())

let resume_verified_exchange ?(persist = persist_delivery) ~db
    ~(config : Runtime_config.t)
    (exchange : Github_app_setup_callback.exchange_result) =
  match ensure_retry ~db exchange with
  | Error error ->
      audit_retry_failure ~db ~exchange ~action:"callback_resume_retry_failed"
        ~error ();
      Error error
  | Ok () -> (
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
          ignore (update_retry ~db ~receipt_id:exchange.receipt_id ~error ());
          audit_retry_failure ~db ~exchange ~action:"callback_resume_failed"
            ~error ();
          Error error
      | Ok result -> (
          let delivery = delivery_of_result result in
          match
            update_retry ~db ~receipt_id:exchange.receipt_id ~delivery
              ~error:"resume completed; delivery pending" ()
          with
          | Error error ->
              audit_retry_failure ~db ~exchange ~plan_id:result.plan.id
                ~action:"callback_resume_retry_failed" ~error ();
              Error error
          | Ok () -> (
              match persist ~db result with
              | Error error ->
                  ignore
                    (update_retry ~db ~receipt_id:exchange.receipt_id ~delivery
                       ~error ());
                  audit_retry_failure ~db ~exchange ~plan_id:result.plan.id
                    ~action:"callback_resume_delivery_failed" ~error ();
                  Error error
              | Ok _delivery -> (
                  match clear_retry ~db ~receipt_id:exchange.receipt_id with
                  | Error error ->
                      audit_retry_failure ~db ~exchange ~plan_id:result.plan.id
                        ~action:"callback_resume_retry_clear_failed" ~error ();
                      Error error
                  | Ok () ->
                      ignore
                        (Github_route_ops.record_audit ~db
                           ~setup_plan_id:result.plan.id
                           ~installation_id:
                             exchange.verified_installation.installation_id
                           ~action:"callback_resumed_confirmable_plan"
                           ~details:
                             (`Assoc
                                [
                                  ( "exchange_receipt_id",
                                    `String exchange.receipt_id );
                                  ( "target",
                                    `String
                                      (match result.target with
                                      | Github_app_setup_resume.Active_room _ ->
                                          "active_room"
                                      | Github_app_setup_resume.Notification _
                                        ->
                                          "notification") );
                                  ("apply", `Bool false);
                                ])
                           ());
                      Ok ()))))

let exchange_of_retry ~db (retry : retry) =
  match Github_app_setup_tx.get ~db ~id:retry.tx_id with
  | Error error -> Error error
  | Ok None -> Error ("retry transaction is missing: " ^ retry.tx_id)
  | Ok (Some transaction) -> (
      match
        Github_app_setup_callback.find_receipt_by_tx ~db ~tx_id:retry.tx_id
      with
      | Error error -> Error error
      | Ok None ->
          Error ("retry receipt is missing for transaction: " ^ retry.tx_id)
      | Ok (Some (receipt_id, app)) -> (
          if receipt_id <> retry.receipt_id then
            Error "retry receipt does not match its transaction"
          else
            match
              Github_app_installation_scope.get ~db
                ~installation_id:retry.installation_id
            with
            | Error error -> Error error
            | Ok None ->
                Error
                  (Printf.sprintf "retry installation scope is missing: %d"
                     retry.installation_id)
            | Ok (Some verified_installation) ->
                Ok
                  {
                    Github_app_setup_callback.transaction;
                    app;
                    installation_id = Some retry.installation_id;
                    verified_installation;
                    raw_app_id = app.app_id;
                    receipt_id;
                  }))

let retry_delivery ~db (retry : retry) =
  match (retry.plan_id, retry.target, retry.message) with
  | Some plan_id, Some target, Some message ->
      persist_delivery_record ~db
        {
          plan_id;
          target;
          room_id = retry.room_id;
          session_key = retry.session_key;
          message;
          created_at = retry.created_at;
        }
  | _ -> Error "retry delivery metadata is incomplete"

let retry_pending ~db ~(config : Runtime_config.t) ?(limit = 100) () =
  list_retries ~db ~limit ()
  |> List.fold_left
       (fun resumed retry ->
         match retry.plan_id with
         | Some _ -> (
             match retry_delivery ~db retry with
             | Ok _ -> (
                 match clear_retry ~db ~receipt_id:retry.receipt_id with
                 | Ok () -> resumed + 1
                 | Error error ->
                     ignore
                       (update_retry ~db ~receipt_id:retry.receipt_id ~error ());
                     resumed)
             | Error error ->
                 ignore
                   (update_retry ~db ~receipt_id:retry.receipt_id ~error ());
                 resumed)
         | None -> (
             match exchange_of_retry ~db retry with
             | Error error ->
                 ignore
                   (update_retry ~db ~receipt_id:retry.receipt_id ~error ());
                 resumed
             | Ok exchange -> (
                 match resume_verified_exchange ~db ~config exchange with
                 | Ok () -> resumed + 1
                 | Error _ -> resumed)))
       0

let install_callback_resume ~db ~current_config =
  Github_app_setup_callback.set_resume_hook (fun exchange ->
      resume_verified_exchange ~db ~config:(current_config ()) exchange)

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
