(* Confirm/apply for Setup_plan: rechecks, CAS store, idempotent receipt,
   redacted audit. See setup_plan_apply.mli. *)

type reject_reason =
  | Plan_not_found
  | Digest_mismatch
  | Principal_mismatch
  | Expired
  | Stale_revision
  | Destination_mismatch
  | Authority_denied
  | Readiness_failed
  | Concurrent_conflict
  | Apply_error

type outcome =
  | Applied of { receipt_id : string; first_time : bool }
  | Rejected of { reason : reject_reason; message : string }

type audit_record = {
  id : int;
  timestamp : string;
  plan_id : string;
  digest : string;
  principal_id : string;
  outcome : string;
  reason : string option;
  details : string;
}

type authority_check =
  principal:Setup_plan.principal ->
  destination:Setup_plan.context ->
  (unit, string) result

type apply_ops = plan:Setup_plan.t -> receipt_id:string -> (unit, string) result

let string_of_reject_reason = function
  | Plan_not_found -> "plan_not_found"
  | Digest_mismatch -> "digest_mismatch"
  | Principal_mismatch -> "principal_mismatch"
  | Expired -> "expired"
  | Stale_revision -> "stale_revision"
  | Destination_mismatch -> "destination_mismatch"
  | Authority_denied -> "authority_denied"
  | Readiness_failed -> "readiness_failed"
  | Concurrent_conflict -> "concurrent_conflict"
  | Apply_error -> "apply_error"

let string_of_outcome = function
  | Applied { receipt_id; first_time } ->
      Printf.sprintf "applied receipt=%s first=%b" receipt_id first_time
  | Rejected { reason; message } ->
      Printf.sprintf "rejected %s: %s" (string_of_reject_reason reason) message

let init_schema db =
  (* Wait for a competing short-lived writer so a rejected/successful apply can
     still durably record its audit rather than silently losing the record. *)
  Sqlite3.busy_timeout db 5_000;
  let plans_sql =
    {|CREATE TABLE IF NOT EXISTS setup_plans (
      id TEXT PRIMARY KEY NOT NULL,
      digest TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      base_revision TEXT NOT NULL,
      destination_room TEXT,
      status TEXT NOT NULL DEFAULT 'pending',
      plan_json TEXT NOT NULL,
      receipt_id TEXT,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      applied_at TEXT,
      updated_at TEXT NOT NULL
    )|}
  in
  let audit_sql =
    {|CREATE TABLE IF NOT EXISTS setup_plan_audit (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      plan_id TEXT NOT NULL,
      digest TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      outcome TEXT NOT NULL,
      reason TEXT,
      details TEXT NOT NULL
    )|}
  in
  let idx_sql =
    {|CREATE INDEX IF NOT EXISTS idx_setup_plan_audit_plan
      ON setup_plan_audit(plan_id)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "setup_plan_apply schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ plans_sql; audit_sql; idx_sql ]

let rejected reason message = Rejected { reason; message }

let receipt_id_for_plan plan_id =
  (* The idempotency key reaches the adapter before the plan row can be marked
     applied. Deriving it from the unique plan id makes crash retries reuse the
     same external idempotency key instead of issuing a second mutation. *)
  "rcpt_" ^ Digest.to_hex (Digest.string plan_id)

let scrub_token_patterns s =
  (* Defense-in-depth for free-text error/audit strings that may embed secrets
     outside keyed JSON fields (e.g. "bot_token=xoxb-..."). *)
  let s =
    Str.global_replace
      (Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" s
  in
  let s =
    Str.global_replace (Str.regexp "xox[baprs]-[A-Za-z0-9-]+") "xox*-REDACTED" s
  in
  let s =
    Str.global_replace
      (Str.regexp_case_fold
         "\\(token\\|secret\\|password\\|api_key\\|private_key\\)[ \\t]*[=:][ \
          \\t]*.*")
      "\\1=[REDACTED]" s
  in
  s

let rec scrub_json_strings = function
  | `String s -> `String (scrub_token_patterns s)
  | `Assoc fields ->
      `Assoc (List.map (fun (k, v) -> (k, scrub_json_strings v)) fields)
  | `List items -> `List (List.map scrub_json_strings items)
  | other -> other

let redact_details_json (j : Yojson.Safe.t) : string =
  j |> Config_show.redact_json |> scrub_json_strings |> Yojson.Safe.to_string

let audit_insert ~db ~plan_id ~digest ~principal_id ~outcome ~reason ~details
    ?(now = Unix.gettimeofday ()) () =
  let ts = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|INSERT INTO setup_plan_audit
      (timestamp, plan_id, digest, principal_id, outcome, reason, details)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT ts);
  bind 2 (Sqlite3.Data.TEXT plan_id);
  bind 3 (Sqlite3.Data.TEXT digest);
  bind 4 (Sqlite3.Data.TEXT principal_id);
  bind 5 (Sqlite3.Data.TEXT outcome);
  bind 6
    (match reason with
    | None -> Sqlite3.Data.NULL
    | Some r -> Sqlite3.Data.TEXT r);
  bind 7 (Sqlite3.Data.TEXT details);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc -> Error (Sqlite3.Rc.to_string rc)

let store_plan ~db (plan : Setup_plan.t) =
  let plan = Setup_plan.redact plan in
  let json = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  let dest_room =
    match plan.destination.room_id with
    | None -> Sqlite3.Data.NULL
    | Some r -> Sqlite3.Data.TEXT r
  in
  let now = Time_util.iso8601_utc () in
  let sql =
    {|INSERT INTO setup_plans
      (id, digest, principal_id, base_revision, destination_room, status,
       plan_json, receipt_id, created_at, expires_at, applied_at, updated_at)
      VALUES (?, ?, ?, ?, ?, 'pending', ?, NULL, ?, ?, NULL, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT plan.id);
  bind 2 (Sqlite3.Data.TEXT plan.digest);
  bind 3 (Sqlite3.Data.TEXT plan.principal.id);
  bind 4 (Sqlite3.Data.TEXT plan.base_revision);
  bind 5 dest_room;
  bind 6 (Sqlite3.Data.TEXT json);
  bind 7 (Sqlite3.Data.TEXT plan.created_at);
  bind 8 (Sqlite3.Data.TEXT plan.expires_at);
  bind 9 (Sqlite3.Data.TEXT now);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | Sqlite3.Rc.CONSTRAINT ->
      Error (Printf.sprintf "plan id already exists: %s" plan.id)
  | rc ->
      Error (Printf.sprintf "store_plan failed: %s" (Sqlite3.Rc.to_string rc))

let get_plan ~db ~plan_id =
  let sql = "SELECT plan_json FROM setup_plans WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT plan_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT json -> (
            match
              Yojson.Safe.from_string json |> Setup_plan.of_persist_json
            with
            | Ok p -> Some p
            | Error _ -> None)
        | _ -> None)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

type stored_row = {
  plan : Setup_plan.t;
  status : string;
  receipt_id : string option;
  base_revision : string;
}

let load_row ~db ~plan_id : stored_row option =
  let sql =
    {|SELECT plan_json, status, receipt_id, base_revision
      FROM setup_plans WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT plan_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        let plan_json =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let status =
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.TEXT s -> s
          | _ -> "pending"
        in
        let receipt_id =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let base_revision =
          match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        match
          Yojson.Safe.from_string plan_json |> Setup_plan.of_persist_json
        with
        | Ok plan -> Some { plan; status; receipt_id; base_revision }
        | Error _ -> None)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let begin_immediate db =
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc -> Error (Printf.sprintf "BEGIN failed: %s" (Sqlite3.Rc.to_string rc))

let commit db =
  match Sqlite3.exec db "COMMIT" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc -> Error (Printf.sprintf "COMMIT failed: %s" (Sqlite3.Rc.to_string rc))

let rollback db = ignore (Sqlite3.exec db "ROLLBACK")

(** CAS: pending → applied only if still pending with matching digest. *)
let cas_mark_applied ~db ~plan_id ~digest ~receipt_id ~now =
  let applied_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE setup_plans
      SET status = 'applied', receipt_id = ?, applied_at = ?, updated_at = ?
      WHERE id = ? AND digest = ? AND status = 'pending'|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT receipt_id);
  bind 2 (Sqlite3.Data.TEXT applied_at);
  bind 3 (Sqlite3.Data.TEXT applied_at);
  bind 4 (Sqlite3.Data.TEXT plan_id);
  bind 5 (Sqlite3.Data.TEXT digest);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      let changes = Sqlite3.changes db in
      if changes = 1 then Ok () else Error "concurrent_conflict"
  | rc -> Error (Sqlite3.Rc.to_string rc)

let audit_reject ~db ~plan_id ~digest ~principal_id ~reason ~message ~extra ~now
    =
  let details =
    redact_details_json
      (`Assoc
         ([
            ("message", `String message);
            ("reason", `String (string_of_reject_reason reason));
          ]
         @ extra))
  in
  audit_insert ~db ~plan_id ~digest ~principal_id ~outcome:"rejected"
    ~reason:(Some (string_of_reject_reason reason))
    ~details ~now ()

let applied_idempotent ~db ~plan_id ~digest ~principal_id ~receipt_id ~now =
  let details =
    redact_details_json
      (`Assoc
         [ ("receipt_id", `String receipt_id); ("plan_id", `String plan_id) ])
  in
  match
    audit_insert ~db ~plan_id ~digest ~principal_id
      ~outcome:"applied_idempotent" ~reason:None ~details ~now ()
  with
  | Ok () -> Applied { receipt_id; first_time = false }
  | Error audit_error ->
      rejected Apply_error
        ("already applied, but could not persist retry audit: " ^ audit_error)

let apply_kind_name = function
  | Setup_plan.Room_profile -> "room_profile"
  | Github_app_setup -> "github_app_setup"
  | Github_route -> "github_route"
  | Access_bundle -> "access_bundle"
  | Generic s -> s

let apply ~db ~plan_id ~digest ~(principal : Setup_plan.principal)
    ~current_base_revision ~destination_room ?(now = Unix.gettimeofday ())
    ~authority ~apply_ops () =
  let principal_id = principal.id in
  let reject reason message extra =
    match
      audit_reject ~db ~plan_id ~digest ~principal_id ~reason ~message ~extra
        ~now
    with
    | Ok () -> rejected reason message
    | Error audit_error ->
        rejected Apply_error
          ("could not persist required rejection audit: " ^ audit_error)
  in
  match load_row ~db ~plan_id with
  | None ->
      reject Plan_not_found "plan not found" [ ("plan_id", `String plan_id) ]
  | Some row -> (
      let plan = row.plan in
      (* 1. Identity checks (always). *)
      if not (Setup_plan.digests_equal plan.digest digest) then
        reject Digest_mismatch "plan digest does not match"
          [
            ( "expected_prefix",
              `String
                (String.sub plan.digest 0 (min 8 (String.length plan.digest)))
            );
          ]
      else if plan.principal.id <> principal_id then
        reject Principal_mismatch
          "apply principal does not match plan principal"
          [
            ("plan_principal", `String plan.principal.id);
            ("apply_principal", `String principal_id);
          ]
      else if
        (* 2. Already-applied short-circuit: retry-idempotent. Does not recheck
           expiry / base_revision / authority so post-apply state advances still
           return the original receipt. *)
        row.status = "applied"
      then
        match row.receipt_id with
        | Some receipt_id ->
            applied_idempotent ~db ~plan_id ~digest ~principal_id ~receipt_id
              ~now
        | None -> reject Concurrent_conflict "applied plan missing receipt" []
      else if row.status <> "pending" then
        reject Concurrent_conflict
          (Printf.sprintf "plan status is %s" row.status)
          [ ("status", `String row.status) ]
      else if
        (* 3. Live rechecks for first apply only. *)
        Setup_plan.is_expired ~now plan
      then
        reject Expired "plan has expired"
          [ ("expires_at", `String plan.expires_at) ]
      else if plan.base_revision <> current_base_revision then
        reject Stale_revision "base revision no longer matches current state"
          [
            ("plan_base_revision", `String plan.base_revision);
            ("current_base_revision", `String current_base_revision);
          ]
      else if plan.destination.room_id <> Some destination_room then
        reject Destination_mismatch "destination room does not match expected"
          [
            ( "plan_destination",
              match plan.destination.room_id with
              | None -> `Null
              | Some r -> `String r );
            ("expected", `String destination_room);
          ]
      else if not (Setup_plan.readiness_ok plan) then
        reject Readiness_failed "plan readiness has failing checks" []
      else
        match authority ~principal ~destination:plan.destination with
        | Error msg ->
            reject Authority_denied msg [ ("authority_message", `String msg) ]
        | Ok () -> (
            (* 4. Atomic apply under IMMEDIATE lock. *)
            match begin_immediate db with
            | Error e -> reject Apply_error e []
            | Ok () -> (
                match load_row ~db ~plan_id with
                | None ->
                    rollback db;
                    reject Plan_not_found "plan disappeared" []
                | Some locked when locked.status = "applied" -> (
                    rollback db;
                    match locked.receipt_id with
                    | Some receipt_id ->
                        applied_idempotent ~db ~plan_id ~digest ~principal_id
                          ~receipt_id ~now
                    | None ->
                        reject Concurrent_conflict
                          "applied plan missing receipt" [])
                | Some locked when locked.status <> "pending" ->
                    rollback db;
                    reject Concurrent_conflict
                      (Printf.sprintf "plan status is %s" locked.status)
                      []
                | Some locked when locked.base_revision <> current_base_revision
                  ->
                    rollback db;
                    reject Stale_revision "base revision changed under lock" []
                | Some _locked -> (
                    let receipt_id = receipt_id_for_plan plan_id in
                    (* Domain ops then durable CAS. apply_ops must tolerate
                       retry with this stable receipt_id if crash occurs after
                       domain success but before commit. *)
                    match apply_ops ~plan ~receipt_id with
                    | Error err -> (
                        rollback db;
                        let details =
                          redact_details_json
                            (`Assoc
                               [
                                 ("error", `String err);
                                 ("plan_id", `String plan_id);
                               ])
                        in
                        match
                          audit_insert ~db ~plan_id ~digest ~principal_id
                            ~outcome:"failed" ~reason:(Some "apply_error")
                            ~details ~now ()
                        with
                        | Ok () -> rejected Apply_error err
                        | Error audit_error ->
                            rejected Apply_error
                              ("apply failed, but could not persist required \
                                failure audit: " ^ audit_error))
                    | Ok () -> (
                        match
                          cas_mark_applied ~db ~plan_id ~digest:plan.digest
                            ~receipt_id ~now
                        with
                        | Error "concurrent_conflict" ->
                            rollback db;
                            reject Concurrent_conflict "lost CAS race on apply"
                              []
                        | Error e ->
                            rollback db;
                            reject Apply_error e []
                        | Ok () -> (
                            (* Success audit inside the same transaction. *)
                            let details =
                              redact_details_json
                                (`Assoc
                                   [
                                     ("receipt_id", `String receipt_id);
                                     ("plan_id", `String plan_id);
                                     ( "destination_room",
                                       `String destination_room );
                                     ( "apply_kind",
                                       `String
                                         (apply_kind_name
                                            plan.apply_payload.kind) );
                                   ])
                            in
                            match
                              audit_insert ~db ~plan_id ~digest ~principal_id
                                ~outcome:"applied" ~reason:None ~details ~now ()
                            with
                            | Error audit_error ->
                                rollback db;
                                reject Apply_error
                                  ("could not persist required success audit: "
                                 ^ audit_error)
                                  []
                            | Ok () -> (
                                match commit db with
                                | Error e ->
                                    rollback db;
                                    reject Apply_error e []
                                | Ok () ->
                                    Applied { receipt_id; first_time = true })))
                    ))))

let list_audit ~db ?plan_id ?(limit = 100) () =
  let sql, bind_plan =
    match plan_id with
    | None ->
        ( {|SELECT id, timestamp, plan_id, digest, principal_id, outcome, reason, details
            FROM setup_plan_audit ORDER BY id DESC LIMIT ?|},
          false )
    | Some _ ->
        ( {|SELECT id, timestamp, plan_id, digest, principal_id, outcome, reason, details
            FROM setup_plan_audit WHERE plan_id = ? ORDER BY id DESC LIMIT ?|},
          true )
  in
  let stmt = Sqlite3.prepare db sql in
  ((if bind_plan then
      match plan_id with
      | Some pid -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT pid))
      | None -> ());
   let lim_idx = if bind_plan then 2 else 1 in
   ignore (Sqlite3.bind stmt lim_idx (Sqlite3.Data.INT (Int64.of_int limit))));
  let rows = ref [] in
  let rec loop () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let col i = Sqlite3.column stmt i in
        let text i = match col i with Sqlite3.Data.TEXT s -> s | _ -> "" in
        let int_id =
          match col 0 with Sqlite3.Data.INT n -> Int64.to_int n | _ -> 0
        in
        let reason =
          match col 6 with Sqlite3.Data.TEXT s -> Some s | _ -> None
        in
        rows :=
          {
            id = int_id;
            timestamp = text 1;
            plan_id = text 2;
            digest = text 3;
            principal_id = text 4;
            outcome = text 5;
            reason;
            details = text 7;
          }
          :: !rows;
        loop ()
    | _ -> ()
  in
  loop ();
  ignore (Sqlite3.finalize stmt);
  List.rev !rows
