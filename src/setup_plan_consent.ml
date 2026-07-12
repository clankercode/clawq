(* Current-Room / cross-Room admin consent for setup plans. *)

type admin_role = Global_admin | Room_admin of string | None_

type actor = {
  principal_id : string;
  role : admin_role;
  source_room_id : string option;
}

type consent_signal =
  | Explicit_confirm
  | Natural_language
  | External_callback
  | Other of string

type decision =
  | Allow of { reason : string }
  | Deny of { reason : string; code : string }

type consent_record = {
  id : string;
  destination_room_id : string;
  principal_id : string;
  plan_id : string option;
  granted_at : string;
  expires_at : string;
  signal : string;
}

let default_consent_ttl = 3600.0

let signal_counts_as_confirm = function
  | Explicit_confirm -> true
  | Natural_language | External_callback | Other _ -> false

let signal_to_string = function
  | Explicit_confirm -> "explicit_confirm"
  | Natural_language -> "natural_language"
  | External_callback -> "external_callback"
  | Other s -> "other:" ^ s

let string_of_decision = function
  | Allow { reason } -> "allow: " ^ reason
  | Deny { reason; code } -> Printf.sprintf "deny[%s]: %s" code reason

let is_cross_room ~source_room_id ~destination_room_id =
  match (source_room_id, destination_room_id) with
  | Some s, Some d -> s <> d
  | None, Some _ -> true
  | _, None -> false

let is_room_admin_for role room_id =
  match role with
  | Global_admin -> true
  | Room_admin r -> r = room_id
  | None_ -> false

let consent_valid ?(now = Unix.gettimeofday ()) (c : consent_record) =
  let now_s = Time_util.iso8601_utc ~t:now () in
  String.compare now_s c.expires_at <= 0

let evaluate ~(actor : actor) ~destination_room_id
    ?(consent : consent_record option = None) ?(now = Unix.gettimeofday ()) () =
  match actor.role with
  | None_ ->
      Deny
        {
          code = "not_admin";
          reason = "actor is neither Room-admin nor global-admin";
        }
  | Global_admin -> Allow { reason = "global-admin may target any Room" }
  | Room_admin admin_room -> (
      match destination_room_id with
      | None ->
          Deny
            {
              code = "missing_destination";
              reason = "destination Room is required";
            }
      | Some dest when dest = admin_room ->
          (* Current-Room (or admin of destination). *)
          Allow { reason = "Room-admin for destination Room" }
      | Some dest -> (
          (* Cross-Room: require destination admin consent. *)
          match consent with
          | Some c
            when c.destination_room_id = dest
                 && consent_valid ~now c
                 && c.signal = "explicit_confirm" ->
              Allow
                {
                  reason =
                    Printf.sprintf
                      "cross-Room with explicit destination consent from %s"
                      c.principal_id;
                }
          | Some _ ->
              Deny
                {
                  code = "consent_invalid";
                  reason =
                    "destination consent missing, expired, or not explicit";
                }
          | None ->
              Deny
                {
                  code = "cross_room_consent_required";
                  reason =
                    Printf.sprintf
                      "cross-Room apply to %s requires destination Room-admin \
                       consent (NL/callback do not count)"
                      dest;
                }))

let init_schema db =
  let sql =
    {|CREATE TABLE IF NOT EXISTS setup_plan_consents (
      id TEXT PRIMARY KEY NOT NULL,
      destination_room_id TEXT NOT NULL,
      principal_id TEXT NOT NULL,
      plan_id TEXT,
      granted_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      signal TEXT NOT NULL
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_setup_plan_consents_dest
      ON setup_plan_consents(destination_room_id)|}
  in
  List.iter
    (fun s ->
      match Sqlite3.exec db s with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "setup_plan_consent schema: %s"
               (Sqlite3.Rc.to_string rc)))
    [ sql; idx ]

let grant_consent ~db ~destination_room_id ~principal_id ?plan_id ~signal
    ?(ttl_seconds = default_consent_ttl) ?(now = Unix.gettimeofday ()) () =
  if not (signal_counts_as_confirm signal) then
    Error
      (Printf.sprintf
         "consent signal %s never counts as confirmation (only \
          explicit_confirm)"
         (signal_to_string signal))
  else
    let id =
      Printf.sprintf "consent_%d_%06d" (int_of_float now) (Random.int 1_000_000)
    in
    let granted_at = Time_util.iso8601_utc ~t:now () in
    let expires_at = Time_util.iso8601_utc ~t:(now +. ttl_seconds) () in
    let rec_ =
      {
        id;
        destination_room_id;
        principal_id;
        plan_id;
        granted_at;
        expires_at;
        signal = "explicit_confirm";
      }
    in
    let sql =
      {|INSERT INTO setup_plan_consents
        (id, destination_room_id, principal_id, plan_id, granted_at, expires_at, signal)
        VALUES (?, ?, ?, ?, ?, ?, ?)|}
    in
    let stmt = Sqlite3.prepare db sql in
    let bind i v = ignore (Sqlite3.bind stmt i v) in
    bind 1 (Sqlite3.Data.TEXT rec_.id);
    bind 2 (Sqlite3.Data.TEXT destination_room_id);
    bind 3 (Sqlite3.Data.TEXT principal_id);
    bind 4
      (match plan_id with
      | None -> Sqlite3.Data.NULL
      | Some p -> Sqlite3.Data.TEXT p);
    bind 5 (Sqlite3.Data.TEXT granted_at);
    bind 6 (Sqlite3.Data.TEXT expires_at);
    bind 7 (Sqlite3.Data.TEXT rec_.signal);
    let rc = Sqlite3.step stmt in
    ignore (Sqlite3.finalize stmt);
    match rc with
    | Sqlite3.Rc.DONE -> Ok rec_
    | rc ->
        Error
          (Printf.sprintf "grant_consent failed: %s" (Sqlite3.Rc.to_string rc))

let find_valid_consent ~db ~destination_room_id ?plan_id
    ?(now = Unix.gettimeofday ()) () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let sql, has_plan =
    match plan_id with
    | None ->
        ( {|SELECT id, destination_room_id, principal_id, plan_id, granted_at,
                  expires_at, signal
           FROM setup_plan_consents
           WHERE destination_room_id = ? AND signal = 'explicit_confirm'
             AND expires_at >= ?
           ORDER BY granted_at DESC LIMIT 1|},
          false )
    | Some _ ->
        ( {|SELECT id, destination_room_id, principal_id, plan_id, granted_at,
                  expires_at, signal
           FROM setup_plan_consents
           WHERE destination_room_id = ? AND signal = 'explicit_confirm'
             AND expires_at >= ?
             AND (plan_id IS NULL OR plan_id = ?)
           ORDER BY granted_at DESC LIMIT 1|},
          true )
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT destination_room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT now_s));
  (if has_plan then
     match plan_id with
     | Some p -> ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT p))
     | None -> ());
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let text i =
          match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let plan_id_opt =
          match Sqlite3.column stmt 3 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        Some
          {
            id = text 0;
            destination_room_id = text 1;
            principal_id = text 2;
            plan_id = plan_id_opt;
            granted_at = text 4;
            expires_at = text 5;
            signal = text 6;
          }
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let authority_check ~db ~(actor : actor) ?(now = Unix.gettimeofday ()) () :
    Setup_plan_apply.authority_check =
 fun ~principal ~destination ->
  (* Principal on the plan must match the acting principal. *)
  if principal.id <> actor.principal_id then
    Error "apply principal does not match consent actor"
  else
    let dest_room = destination.room_id in
    let consent_opt =
      match dest_room with
      | None -> None
      | Some room -> find_valid_consent ~db ~destination_room_id:room ~now ()
    in
    match
      evaluate ~actor ~destination_room_id:dest_room ~consent:consent_opt ~now
        ()
    with
    | Allow _ -> Ok ()
    | Deny { reason; _ } -> Error reason
