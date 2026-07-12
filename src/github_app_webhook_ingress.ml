(* Verified shared GitHub App webhook ingress with durable delivery identity
   (P19.M2.E1.T004). See github_app_webhook_ingress.mli. *)

type headers = {
  delivery_id : string option;
  event : string option;
  signature_header : string option;
  user_agent : string option;
}

type request = { body : string; headers : headers; path : string }

type reject_reason =
  | Bad_signature
  | Missing_delivery_id
  | Duplicate_delivery
  | Unknown_or_suspended_installation
  | Repo_not_in_scope
  | Event_not_subscribed
  | Invalid_payload
  | Wrong_path
  | App_id_mismatch

type accepted = {
  delivery_id : string;
  event : string;
  installation_id : int option;
  app_id : int option;
  repo_full_name : string option;
  action : string option;
  payload : Yojson.Safe.t;
}

type outcome =
  | Accepted of accepted
  | Rejected of { reason : reject_reason; message : string }
  | Duplicate of { delivery_id : string }

let default_path = Github_app_setup_tx.default_hook_path
let default_allowed_events = Github_app_setup_tx.default_events

let reject_reason_to_string = function
  | Bad_signature -> "bad_signature"
  | Missing_delivery_id -> "missing_delivery_id"
  | Duplicate_delivery -> "duplicate_delivery"
  | Unknown_or_suspended_installation -> "unknown_or_suspended_installation"
  | Repo_not_in_scope -> "repo_not_in_scope"
  | Event_not_subscribed -> "event_not_subscribed"
  | Invalid_payload -> "invalid_payload"
  | Wrong_path -> "wrong_path"
  | App_id_mismatch -> "app_id_mismatch"

let rejected reason message = Rejected { reason; message }

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_webhook_deliveries (
      delivery_id TEXT PRIMARY KEY NOT NULL,
      received_at TEXT NOT NULL,
      event TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'accepted'
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_webhook_deliveries_received
      ON github_app_webhook_deliveries(received_at)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_app_webhook_ingress schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx ]

let was_seen ~db ~delivery_id =
  let sql =
    {|SELECT 1 FROM github_app_webhook_deliveries WHERE delivery_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT delivery_id));
  let seen =
    match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false
  in
  ignore (Sqlite3.finalize stmt);
  seen

let insert_delivery ~db ~delivery_id ~event ~received_at =
  let sql =
    {|INSERT INTO github_app_webhook_deliveries
        (delivery_id, received_at, event, status)
      VALUES (?, ?, ?, 'accepted')|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT delivery_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT received_at));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT event));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | Sqlite3.Rc.CONSTRAINT -> Error `Duplicate
  | rc ->
      Error
        (`Db
           (Printf.sprintf "insert delivery failed: %s (%s)"
              (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let record_ack ~db ~delivery_id =
  if String.trim delivery_id = "" then Error "empty delivery_id"
  else
    let sql =
      {|UPDATE github_app_webhook_deliveries SET status = 'acked'
        WHERE delivery_id = ?|}
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT delivery_id));
    let rc = Sqlite3.step stmt in
    ignore (Sqlite3.finalize stmt);
    match rc with
    | Sqlite3.Rc.DONE ->
        if Sqlite3.changes db = 0 then
          Error (Printf.sprintf "delivery_id not in ledger: %s" delivery_id)
        else Ok ()
    | rc ->
        Error
          (Printf.sprintf "record_ack failed: %s (%s)" (Sqlite3.Rc.to_string rc)
             (Sqlite3.errmsg db))

let json_int = function
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | `Float f when Float.is_integer f -> Some (int_of_float f)
  | _ -> None

let extract_fields (payload : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let action =
    match member "action" payload with `String s -> Some s | _ -> None
  in
  let installation = member "installation" payload in
  let installation_id =
    match installation with `Null -> None | j -> json_int (member "id" j)
  in
  let app_id_from_installation =
    match installation with `Null -> None | j -> json_int (member "app_id" j)
  in
  let app_id_from_root = json_int (member "app_id" payload) in
  let app_id =
    match app_id_from_installation with
    | Some _ as id -> id
    | None -> app_id_from_root
  in
  let repo_full_name =
    match member "repository" payload with
    | `Null -> None
    | repo -> (
        match member "full_name" repo with
        | `String s when String.trim s <> "" -> Some s
        | _ -> None)
  in
  (installation_id, app_id, repo_full_name, action)

let is_installation_event event =
  event = "installation" || event = "installation_repositories"

let event_allowed ~allowed_events ~event =
  event = "ping" || List.exists (fun e -> String.equal e event) allowed_events

let normalize_path p =
  let p = String.trim p in
  (* Drop query string if present. *)
  match String.index_opt p '?' with
  | Some i -> String.sub p 0 i
  | None -> p

let verify_and_accept ~db ~webhook_secret ?(expected_path = default_path)
    ?(allowed_events = default_allowed_events) ?expected_app_id
    ?(now = Unix.gettimeofday ()) (req : request) =
  let path = normalize_path req.path in
  let expected = normalize_path expected_path in
  if path <> expected then
    rejected Wrong_path
      (Printf.sprintf "path %S does not match expected %S" path expected)
  else
    let sig_hdr =
      match req.headers.signature_header with
      | Some s when String.trim s <> "" -> s
      | _ -> ""
    in
    if
      not
        (Github_webhook.verify_signature ~secret:webhook_secret ~body:req.body
           ~signature_header:sig_hdr)
    then rejected Bad_signature "invalid or missing X-Hub-Signature-256"
    else
      match req.headers.delivery_id with
      | None | Some "" ->
          rejected Missing_delivery_id "missing X-GitHub-Delivery"
      | Some delivery_id when String.trim delivery_id = "" ->
          rejected Missing_delivery_id "missing X-GitHub-Delivery"
      | Some delivery_id -> (
          if was_seen ~db ~delivery_id then Duplicate { delivery_id }
          else
            let event =
              match req.headers.event with
              | Some e when String.trim e <> "" -> String.trim e
              | _ -> ""
            in
            if event = "" then rejected Invalid_payload "missing X-GitHub-Event"
            else if not (event_allowed ~allowed_events ~event) then
              rejected Event_not_subscribed
                (Printf.sprintf "event %S is not subscribed" event)
            else
              let payload_result =
                try Ok (Yojson.Safe.from_string req.body) with
                | Yojson.Json_error msg -> Error msg
                | _ -> Error "invalid JSON body"
              in
              match payload_result with
              | Error msg ->
                  rejected Invalid_payload
                    (Printf.sprintf "invalid JSON payload: %s" msg)
              | Ok payload -> (
                  let installation_id, app_id, repo_full_name, action =
                    extract_fields payload
                  in
                  let app_ok =
                    match (expected_app_id, app_id) with
                    | Some want, Some got when want <> got -> false
                    | _ -> true
                  in
                  if not app_ok then
                    rejected App_id_mismatch
                      (Printf.sprintf
                         "payload app_id does not match expected %d"
                         (Option.get expected_app_id))
                  else
                    let scope_result =
                      if is_installation_event event then Ok ()
                      else
                        match installation_id with
                        | None -> Ok ()
                        | Some iid -> (
                            match
                              Github_app_installation_scope.get ~db
                                ~installation_id:iid
                            with
                            | Error e ->
                                Error
                                  ( Unknown_or_suspended_installation,
                                    Printf.sprintf
                                      "installation lookup failed: %s" e )
                            | Ok None ->
                                Error
                                  ( Unknown_or_suspended_installation,
                                    Printf.sprintf "unknown installation_id %d"
                                      iid )
                            | Ok (Some scope) -> (
                                match scope.status with
                                | Github_app_installation_scope.Suspended _
                                | Github_app_installation_scope.Deleted ->
                                    Error
                                      ( Unknown_or_suspended_installation,
                                        Printf.sprintf "installation %d is %s"
                                          iid
                                          (Github_app_installation_scope
                                           .status_to_string scope.status) )
                                | Github_app_installation_scope.Active -> (
                                    match repo_full_name with
                                    | None -> Ok ()
                                    | Some repo ->
                                        if
                                          Github_app_installation_scope
                                          .is_repo_authorized scope
                                            ~repo_full_name:repo
                                        then Ok ()
                                        else
                                          Error
                                            ( Repo_not_in_scope,
                                              Printf.sprintf
                                                "repository %S not in \
                                                 installation %d scope"
                                                repo iid ))))
                    in
                    match scope_result with
                    | Error (reason, message) -> rejected reason message
                    | Ok () -> (
                        let received_at = Time_util.iso8601_utc ~t:now () in
                        match
                          insert_delivery ~db ~delivery_id ~event ~received_at
                        with
                        | Error `Duplicate -> Duplicate { delivery_id }
                        | Error (`Db msg) ->
                            rejected Invalid_payload
                              (Printf.sprintf "ledger write failed: %s" msg)
                        | Ok () ->
                            Accepted
                              {
                                delivery_id;
                                event;
                                installation_id;
                                app_id;
                                repo_full_name;
                                action;
                                payload;
                              })))
