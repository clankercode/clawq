(* Durable leased GitHub App device-code polling (P21.M2.E3.T002).
   See github_user_auth_device_poll.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Dev = Github_user_auth_device
module Tx = Github_user_auth_tx

let schema_version = 1
let default_lease_seconds = 30.0
let slow_down_extra_seconds = 5
let access_token_path = "/login/oauth/access_token"
let device_grant_type = "urn:ietf:params:oauth:grant-type:device_code"
let default_host = "github.com"

type http_post = Dev.http_post
type resolve_client_id = Dev.resolve_client_id
type refuse_error = Dev.refuse_error

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type stop_reason =
  | Cancelled
  | Expired
  | Access_denied
  | Device_code_expired
  | Access_granted
  | Unsupported_grant
  | Incorrect_device_code
  | Terminal of string

type token_success = {
  access_token : string;
  token_type : string option;
  scope : string option;
  expires_in : int option;
  refresh_token : string option;
}

type token_error = {
  error : string;
  error_description : string option;
  interval : int option;
}

type token_response =
  | Token_success of token_success
  | Token_error of token_error

type poll_state = {
  session_id : string;
  interval_seconds : int;
  expires_at : string;
  next_poll_at : string;
  poll_lease_owner : string option;
  poll_lease_token : string option;
  poll_lease_expires_at : string option;
  poll_stop_reason : stop_reason option;
  updated_at : string;
}

type lease = {
  session_id : string;
  worker_id : string;
  token : string;
  lease_expires_at : string;
}

type pending_timing = {
  session : Dev.session;
  interval_seconds : int;
  next_poll_at : string;
}

type granted = { session : Dev.session; tokens : token_success }

type poll_outcome =
  | Authorization_pending of pending_timing
  | Slow_down of pending_timing
  | Granted of granted
  | Stopped of {
      session : Dev.session option;
      reason : stop_reason;
      message : string;
    }
  | Not_due of { session : Dev.session; next_poll_at : string }
  | Lease_busy of { session_id : string; owner : string option }

(* -------------------------------------------------------------------------- *)
(* Codecs                                                                     *)
(* -------------------------------------------------------------------------- *)

let string_of_stop_reason = function
  | Cancelled -> "cancelled"
  | Expired -> "expired"
  | Access_denied -> "access_denied"
  | Device_code_expired -> "device_code_expired"
  | Access_granted -> "access_granted"
  | Unsupported_grant -> "unsupported_grant"
  | Incorrect_device_code -> "incorrect_device_code"
  | Terminal s -> "terminal:" ^ s

let stop_reason_of_string = function
  | "cancelled" -> Ok Cancelled
  | "expired" -> Ok Expired
  | "access_denied" -> Ok Access_denied
  | "device_code_expired" | "expired_token" -> Ok Device_code_expired
  | "access_granted" -> Ok Access_granted
  | "unsupported_grant" | "unsupported_grant_type" -> Ok Unsupported_grant
  | "incorrect_device_code" -> Ok Incorrect_device_code
  | s when String.length s > 9 && String.sub s 0 9 = "terminal:" ->
      Ok (Terminal (String.sub s 9 (String.length s - 9)))
  | s -> Error (Printf.sprintf "unknown poll stop_reason: %s" s)

let stop_reason_of_github_error err =
  match String.lowercase_ascii (String.trim err) with
  | "authorization_pending" -> None
  | "slow_down" -> None
  | "access_denied" -> Some Access_denied
  | "expired_token" -> Some Device_code_expired
  | "unsupported_grant_type" -> Some Unsupported_grant
  | "incorrect_device_code" -> Some Incorrect_device_code
  | other -> Some (Terminal other)

(* -------------------------------------------------------------------------- *)
(* Time helpers                                                               *)
(* -------------------------------------------------------------------------- *)

let now_iso ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

let iso_after ~now ~seconds =
  Time_util.iso8601_utc ~t:(now +. float_of_int seconds) ()

let is_due ~now iso =
  let now_s = now_iso ~now () in
  String.compare now_s iso >= 0

(* -------------------------------------------------------------------------- *)
(* URL                                                                        *)
(* -------------------------------------------------------------------------- *)

let normalize_host host =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  match String.split_on_char '/' h with
  | host_part :: _ -> (
      match String.split_on_char ':' host_part with
      | host_only :: _ -> host_only
      | [] -> "")
  | [] -> ""

let access_token_url ?(host = default_host) () =
  let host = normalize_host host in
  let host = if host = "" then default_host else host in
  Printf.sprintf "https://%s%s" host access_token_path

(* -------------------------------------------------------------------------- *)
(* Parse token response                                                       *)
(* -------------------------------------------------------------------------- *)

let form_assoc body =
  try
    Uri.query_of_encoded body
    |> List.filter_map (fun (k, vs) ->
        match vs with
        | v :: _ -> Some (String.lowercase_ascii (String.trim k), v)
        | [] -> None)
  with _ -> []

let json_string_opt name j =
  match Yojson.Safe.Util.member name j with
  | `String s ->
      let t = String.trim s in
      if t = "" then None else Some t
  | _ -> None

let json_int_opt name j =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Some i
  | `Intlit s -> ( try Some (int_of_string s) with Failure _ -> None)
  | `String s -> ( try Some (int_of_string (String.trim s)) with _ -> None)
  | _ -> None

let assoc_find key pairs =
  List.find_map
    (fun (k, v) -> if String.equal k key then Some v else None)
    pairs

let assoc_int key pairs =
  match assoc_find key pairs with
  | None -> None
  | Some s -> ( try Some (int_of_string (String.trim s)) with _ -> None)

let nonempty_opt = function
  | None -> None
  | Some s ->
      let t = String.trim s in
      if t = "" then None else Some t

let build_success ~access_token ~token_type ~scope ~expires_in ~refresh_token =
  let access_token = String.trim access_token in
  if access_token = "" then Error "access_token missing from token response"
  else
    Ok
      (Token_success
         {
           access_token;
           token_type = nonempty_opt token_type;
           scope = nonempty_opt scope;
           expires_in;
           refresh_token = nonempty_opt refresh_token;
         })

let build_error ~error ~error_description ~interval =
  let error = String.trim error in
  if error = "" then Error "token error missing error field"
  else
    Ok
      (Token_error
         {
           error;
           error_description = nonempty_opt error_description;
           interval =
             (match interval with Some n when n > 0 -> Some n | _ -> None);
         })

let parse_token_response ~body =
  let body = String.trim body in
  if body = "" then Error "empty token response"
  else if String.length body > 0 && body.[0] = '{' then
    try
      let j = Yojson.Safe.from_string body in
      match json_string_opt "access_token" j with
      | Some access_token ->
          build_success ~access_token
            ~token_type:(json_string_opt "token_type" j)
            ~scope:(json_string_opt "scope" j)
            ~expires_in:(json_int_opt "expires_in" j)
            ~refresh_token:(json_string_opt "refresh_token" j)
      | None -> (
          match json_string_opt "error" j with
          | Some error ->
              build_error ~error
                ~error_description:(json_string_opt "error_description" j)
                ~interval:(json_int_opt "interval" j)
          | None -> Error "token JSON missing access_token or error")
    with _ -> Error "token response is not valid JSON"
  else
    let pairs = form_assoc body in
    match assoc_find "access_token" pairs with
    | Some access_token ->
        build_success ~access_token
          ~token_type:(assoc_find "token_type" pairs)
          ~scope:(assoc_find "scope" pairs)
          ~expires_in:(assoc_int "expires_in" pairs)
          ~refresh_token:(assoc_find "refresh_token" pairs)
    | None -> (
        match assoc_find "error" pairs with
        | Some error ->
            build_error ~error
              ~error_description:(assoc_find "error_description" pairs)
              ~interval:(assoc_int "interval" pairs)
        | None -> Error "token response missing access_token or error")

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let refuse_storage msg : refuse_error =
  { Dev.reason = Dev.Storage msg; message = msg; room_safe_progress = None }

let try_alter db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | _ ->
      (* Column may already exist on a fresh CREATE that includes it. *)
      ()

let ensure_schema db =
  Dev.ensure_schema db;
  (* Additive poll-lease columns on github_user_auth_device. *)
  try_alter db
    "ALTER TABLE github_user_auth_device ADD COLUMN poll_lease_owner TEXT";
  try_alter db
    "ALTER TABLE github_user_auth_device ADD COLUMN poll_lease_token TEXT";
  try_alter db
    "ALTER TABLE github_user_auth_device ADD COLUMN poll_lease_expires_at TEXT";
  try_alter db
    "ALTER TABLE github_user_auth_device ADD COLUMN poll_stop_reason TEXT";
  match
    Sqlite3.exec db
      {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_device_next_poll
        ON github_user_auth_device(next_poll_at)|}
  with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_user_auth_device_poll schema error: %s"
           (Sqlite3.Rc.to_string rc))

(* -------------------------------------------------------------------------- *)
(* Row helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let opt_text s =
  let t = String.trim s in
  if t = "" then None else Some t

let generate_lease_token () =
  let a = Random.bits () land 0x3fffffff in
  let b = Random.bits () land 0x3fffffff in
  Printf.sprintf "devpoll_%08x%08x" a b

let get_poll_state ~db ~session_id =
  let sql =
    {|SELECT id, interval_seconds, expires_at, next_poll_at,
             poll_lease_owner, poll_lease_token, poll_lease_expires_at,
             poll_stop_reason, updated_at
      FROM github_user_auth_device WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> Ok None
    | Sqlite3.Rc.ROW ->
        let stop_raw = opt_text (text_col stmt 7) in
        let poll_stop_reason =
          match stop_raw with
          | None -> None
          | Some s -> (
              match stop_reason_of_string s with
              | Ok r -> Some r
              | Error _ -> Some (Terminal s))
        in
        Ok
          (Some
             {
               session_id = text_col stmt 0;
               interval_seconds = int_col stmt 1;
               expires_at = text_col stmt 2;
               next_poll_at = text_col stmt 3;
               poll_lease_owner = opt_text (text_col stmt 4);
               poll_lease_token = opt_text (text_col stmt 5);
               poll_lease_expires_at = opt_text (text_col stmt 6);
               poll_stop_reason;
               updated_at = text_col stmt 8;
             })
    | rc ->
        Error
          (refuse_storage
             (Printf.sprintf "poll_state load failed: %s"
                (Sqlite3.Rc.to_string rc)))
  in
  ignore (Sqlite3.finalize stmt);
  result

let is_stopped (p : poll_state) = Option.is_some p.poll_stop_reason

let redacted_poll_state (p : poll_state) =
  Printf.sprintf
    "poll_state id=%s interval=%ds expires_at=%s next_poll_at=%s \
     lease_owner=%s stop=%s"
    p.session_id p.interval_seconds p.expires_at p.next_poll_at
    (match p.poll_lease_owner with None -> "-" | Some o -> o)
    (match p.poll_stop_reason with
    | None -> "-"
    | Some r -> string_of_stop_reason r)

let redacted_outcome = function
  | Authorization_pending t ->
      Printf.sprintf "authorization_pending next_poll_at=%s interval=%d"
        t.next_poll_at t.interval_seconds
  | Slow_down t ->
      Printf.sprintf "slow_down next_poll_at=%s interval=%d" t.next_poll_at
        t.interval_seconds
  | Granted g ->
      Printf.sprintf "granted session=%s token_present=true" g.session.Dev.id
  | Stopped { reason; message; _ } ->
      Printf.sprintf "stopped reason=%s msg=%s"
        (string_of_stop_reason reason)
        message
  | Not_due { next_poll_at; _ } ->
      Printf.sprintf "not_due next_poll_at=%s" next_poll_at
  | Lease_busy { session_id; owner } ->
      Printf.sprintf "lease_busy id=%s owner=%s" session_id
        (match owner with None -> "-" | Some o -> o)

let exec_update ~db ~sql ~params =
  let stmt = Sqlite3.prepare db sql in
  List.iteri (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v)) params;
  let rc = Sqlite3.step stmt in
  let changed = Sqlite3.changes db in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok changed
  | rc ->
      Error
        (refuse_storage
           (Printf.sprintf "device poll update failed: %s"
              (Sqlite3.Rc.to_string rc)))

let set_stop_reason ~db ~session_id ~reason ~now =
  let updated_at = now_iso ~now () in
  let reason_s = string_of_stop_reason reason in
  exec_update ~db
    ~sql:
      {|UPDATE github_user_auth_device
        SET poll_stop_reason = ?,
            poll_lease_owner = NULL,
            poll_lease_token = NULL,
            poll_lease_expires_at = NULL,
            updated_at = ?
        WHERE id = ?|}
    ~params:
      [
        Sqlite3.Data.TEXT reason_s;
        Sqlite3.Data.TEXT updated_at;
        Sqlite3.Data.TEXT session_id;
      ]
  |> Result.map (fun _ -> ())

let clear_lease ~db ~session_id ~token ~now =
  let updated_at = now_iso ~now () in
  exec_update ~db
    ~sql:
      {|UPDATE github_user_auth_device
        SET poll_lease_owner = NULL,
            poll_lease_token = NULL,
            poll_lease_expires_at = NULL,
            updated_at = ?
        WHERE id = ? AND poll_lease_token = ?|}
    ~params:
      [
        Sqlite3.Data.TEXT updated_at;
        Sqlite3.Data.TEXT session_id;
        Sqlite3.Data.TEXT token;
      ]
  |> Result.map (fun _ -> ())

let release_lease ~db ~session_id ~token ?(now = Unix.gettimeofday ()) () =
  clear_lease ~db ~session_id ~token ~now

let schedule_next ~db ~session_id ~lease_token ~interval_seconds ~next_poll_at
    ~now ~clear_stop:clear =
  let updated_at = now_iso ~now () in
  let sql =
    if clear then
      {|UPDATE github_user_auth_device
        SET interval_seconds = ?,
            next_poll_at = ?,
            poll_lease_owner = NULL,
            poll_lease_token = NULL,
            poll_lease_expires_at = NULL,
            poll_stop_reason = NULL,
            updated_at = ?
        WHERE id = ? AND poll_lease_token = ?|}
    else
      {|UPDATE github_user_auth_device
        SET interval_seconds = ?,
            next_poll_at = ?,
            poll_lease_owner = NULL,
            poll_lease_token = NULL,
            poll_lease_expires_at = NULL,
            updated_at = ?
        WHERE id = ? AND poll_lease_token = ?|}
  in
  exec_update ~db ~sql
    ~params:
      [
        Sqlite3.Data.INT (Int64.of_int interval_seconds);
        Sqlite3.Data.TEXT next_poll_at;
        Sqlite3.Data.TEXT updated_at;
        Sqlite3.Data.TEXT session_id;
        Sqlite3.Data.TEXT lease_token;
      ]

let stop_and_release ~db ~session_id ~lease_token ~reason ~now =
  let updated_at = now_iso ~now () in
  let reason_s = string_of_stop_reason reason in
  exec_update ~db
    ~sql:
      {|UPDATE github_user_auth_device
        SET poll_stop_reason = ?,
            poll_lease_owner = NULL,
            poll_lease_token = NULL,
            poll_lease_expires_at = NULL,
            updated_at = ?
        WHERE id = ? AND (poll_lease_token = ? OR poll_lease_token IS NULL
                          OR poll_lease_token = '')|}
    ~params:
      [
        Sqlite3.Data.TEXT reason_s;
        Sqlite3.Data.TEXT updated_at;
        Sqlite3.Data.TEXT session_id;
        Sqlite3.Data.TEXT lease_token;
      ]
  |> Result.map (fun _ -> ())

(* -------------------------------------------------------------------------- *)
(* Pre-claim guards (cancel / expiry / stop)                                  *)
(* -------------------------------------------------------------------------- *)

let load_session ~db ~session_id =
  match Dev.get ~db ~id:session_id with
  | Error e -> Error e
  | Ok None ->
      Error
        (refuse_storage
           (Printf.sprintf "device session not found: %s" session_id))
  | Ok (Some s) -> Ok s

let stopped_outcome ~session ~reason ~message =
  Stopped { session = Some session; reason; message }

let check_tx_and_expiry ~db ~(sess : Dev.session) ~now =
  (* Local durable expiry is authoritative across restart — never recompute. *)
  if String.compare (now_iso ~now ()) sess.Dev.expires_at >= 0 then (
    ignore (set_stop_reason ~db ~session_id:sess.Dev.id ~reason:Expired ~now);
    ignore (Tx.expire ~db ~id:sess.Dev.tx_id ~now ());
    Error
      (stopped_outcome ~session:sess ~reason:Expired
         ~message:"device authorization expired; polling stopped"))
  else
    match Tx.get ~db ~id:sess.Dev.tx_id with
    | Error e ->
        Error
          (stopped_outcome ~session:sess ~reason:(Terminal "storage") ~message:e)
    | Ok None ->
        ignore
          (set_stop_reason ~db ~session_id:sess.Dev.id
             ~reason:(Terminal "tx_missing") ~now);
        Error
          (stopped_outcome ~session:sess ~reason:(Terminal "tx_missing")
             ~message:"authorization transaction missing; polling stopped")
    | Ok (Some tx) -> (
        match tx.Tx.status with
        | Tx.Cancelled ->
            ignore
              (set_stop_reason ~db ~session_id:sess.Dev.id ~reason:Cancelled
                 ~now);
            Error
              (stopped_outcome ~session:sess ~reason:Cancelled
                 ~message:"authorization cancelled; polling stopped")
        | Tx.Expired ->
            ignore
              (set_stop_reason ~db ~session_id:sess.Dev.id ~reason:Expired ~now);
            Error
              (stopped_outcome ~session:sess ~reason:Expired
                 ~message:"authorization transaction expired; polling stopped")
        | Tx.Completed | Tx.Superseded | Tx.Rejected ->
            let reason = Terminal (Tx.string_of_status tx.Tx.status) in
            ignore (set_stop_reason ~db ~session_id:sess.Dev.id ~reason ~now);
            Error
              (stopped_outcome ~session:sess ~reason
                 ~message:
                   (Printf.sprintf
                      "authorization transaction is terminal (%s); polling \
                       stopped"
                      (Tx.string_of_status tx.Tx.status)))
        | Tx.Open ->
            if Tx.is_expired ~now tx then (
              ignore (Tx.expire ~db ~id:tx.Tx.id ~now ());
              ignore
                (set_stop_reason ~db ~session_id:sess.Dev.id ~reason:Expired
                   ~now);
              Error
                (stopped_outcome ~session:sess ~reason:Expired
                   ~message:"authorization transaction expired; polling stopped"))
            else Ok ())

(* -------------------------------------------------------------------------- *)
(* try_claim                                                                  *)
(* -------------------------------------------------------------------------- *)

let try_claim ~db ~session_id ~worker_id
    ?(lease_seconds = default_lease_seconds) ?(now = Unix.gettimeofday ()) () =
  let worker_id = String.trim worker_id in
  if worker_id = "" then
    Error
      (Stopped
         {
           session = None;
           reason = Terminal "invalid_worker";
           message = "worker_id must be non-empty";
         })
  else
    match get_poll_state ~db ~session_id with
    | Error e ->
        Error
          (Stopped
             {
               session = None;
               reason = Terminal "storage";
               message = e.Dev.message;
             })
    | Ok None ->
        Error
          (Stopped
             {
               session = None;
               reason = Terminal "not_found";
               message =
                 Printf.sprintf "device session not found: %s" session_id;
             })
    | Ok (Some state) -> (
        match load_session ~db ~session_id with
        | Error e ->
            Error
              (Stopped
                 {
                   session = None;
                   reason = Terminal "storage";
                   message = e.Dev.message;
                 })
        | Ok sess -> (
            match state.poll_stop_reason with
            | Some reason ->
                Error
                  (stopped_outcome ~session:sess ~reason
                     ~message:
                       (Printf.sprintf "device polling already stopped (%s)"
                          (string_of_stop_reason reason)))
            | None -> (
                match check_tx_and_expiry ~db ~sess ~now with
                | Error outcome -> Error outcome
                | Ok () -> (
                    if not (is_due ~now state.next_poll_at) then
                      Error
                        (Not_due
                           { session = sess; next_poll_at = state.next_poll_at })
                    else
                      let token = generate_lease_token () in
                      let now_s = now_iso ~now () in
                      let lease_expires_at =
                        Time_util.iso8601_utc ~t:(now +. lease_seconds) ()
                      in
                      let sql =
                        {|UPDATE github_user_auth_device
                          SET poll_lease_owner = ?,
                              poll_lease_token = ?,
                              poll_lease_expires_at = ?,
                              updated_at = ?
                          WHERE id = ?
                            AND (poll_stop_reason IS NULL OR poll_stop_reason = '')
                            AND next_poll_at <= ?
                            AND expires_at > ?
                            AND (poll_lease_token IS NULL OR poll_lease_token = ''
                                 OR poll_lease_expires_at IS NULL
                                 OR poll_lease_expires_at = ''
                                 OR poll_lease_expires_at < ?)|}
                      in
                      match
                        exec_update ~db ~sql
                          ~params:
                            [
                              Sqlite3.Data.TEXT worker_id;
                              Sqlite3.Data.TEXT token;
                              Sqlite3.Data.TEXT lease_expires_at;
                              Sqlite3.Data.TEXT now_s;
                              Sqlite3.Data.TEXT session_id;
                              Sqlite3.Data.TEXT now_s;
                              Sqlite3.Data.TEXT now_s;
                              Sqlite3.Data.TEXT now_s;
                            ]
                      with
                      | Error e ->
                          Error
                            (Stopped
                               {
                                 session = Some sess;
                                 reason = Terminal "storage";
                                 message = e.Dev.message;
                               })
                      | Ok n when n > 0 ->
                          Ok { session_id; worker_id; token; lease_expires_at }
                      | Ok _ ->
                          (* Lost race or lease still held. *)
                          let owner =
                            match get_poll_state ~db ~session_id with
                            | Ok (Some st) -> st.poll_lease_owner
                            | _ -> state.poll_lease_owner
                          in
                          Error (Lease_busy { session_id; owner })))))

(* -------------------------------------------------------------------------- *)
(* Apply token response                                                       *)
(* -------------------------------------------------------------------------- *)

let reload_session_or ~db ~session_id ~fallback =
  match Dev.get ~db ~id:session_id with Ok (Some s) -> s | _ -> fallback

let apply_token_response ~db ~session_id ~lease_token ~response
    ?(now = Unix.gettimeofday ()) () =
  match load_session ~db ~session_id with
  | Error e -> Error e
  | Ok sess -> (
      match response with
      | Token_success tokens -> (
          match
            stop_and_release ~db ~session_id ~lease_token ~reason:Access_granted
              ~now
          with
          | Error e -> Error e
          | Ok () ->
              let session = reload_session_or ~db ~session_id ~fallback:sess in
              Ok (Granted { session; tokens }))
      | Token_error te -> (
          match stop_reason_of_github_error te.error with
          | Some reason -> (
              match
                stop_and_release ~db ~session_id ~lease_token ~reason ~now
              with
              | Error e -> Error e
              | Ok () ->
                  let session =
                    reload_session_or ~db ~session_id ~fallback:sess
                  in
                  let message =
                    match te.error_description with
                    | Some d -> d
                    | None -> te.error
                  in
                  Ok
                    (Stopped
                       {
                         session = Some session;
                         reason;
                         message =
                           Printf.sprintf "device poll terminal: %s" message;
                       }))
          | None -> (
              let err_l = String.lowercase_ascii (String.trim te.error) in
              if err_l = "authorization_pending" then
                let interval = sess.Dev.interval_seconds in
                let next_poll_at = iso_after ~now ~seconds:interval in
                match
                  schedule_next ~db ~session_id ~lease_token
                    ~interval_seconds:interval ~next_poll_at ~now
                    ~clear_stop:false
                with
                | Error e -> Error e
                | Ok n when n <= 0 ->
                    Error
                      (refuse_storage
                         "lost poll lease while applying authorization_pending")
                | Ok _ ->
                    let session =
                      reload_session_or ~db ~session_id ~fallback:sess
                    in
                    Ok
                      (Authorization_pending
                         { session; interval_seconds = interval; next_poll_at })
              else if err_l = "slow_down" then
                let current = sess.Dev.interval_seconds in
                let interval =
                  match te.interval with
                  | Some n when n > 0 -> n
                  | _ -> current + slow_down_extra_seconds
                in
                let interval = if interval < 1 then 1 else interval in
                let next_poll_at = iso_after ~now ~seconds:interval in
                match
                  schedule_next ~db ~session_id ~lease_token
                    ~interval_seconds:interval ~next_poll_at ~now
                    ~clear_stop:false
                with
                | Error e -> Error e
                | Ok n when n <= 0 ->
                    Error
                      (refuse_storage "lost poll lease while applying slow_down")
                | Ok _ ->
                    let session =
                      reload_session_or ~db ~session_id ~fallback:sess
                    in
                    Ok
                      (Slow_down
                         { session; interval_seconds = interval; next_poll_at })
              else
                (* Unknown non-terminal should not happen after mapping. *)
                let reason = Terminal te.error in
                match
                  stop_and_release ~db ~session_id ~lease_token ~reason ~now
                with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      (Stopped
                         { session = Some sess; reason; message = te.error }))))

(* -------------------------------------------------------------------------- *)
(* HTTP token request                                                         *)
(* -------------------------------------------------------------------------- *)

let request_access_token ~http_post ~host ~client_id ~device_code =
  let url = access_token_url ~host () in
  let body =
    Uri.encoded_of_query
      [
        ("client_id", [ client_id ]);
        ("device_code", [ device_code ]);
        ("grant_type", [ device_grant_type ]);
      ]
  in
  let headers =
    [
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Accept", "application/json");
    ]
  in
  match http_post ~url ~headers ~body with
  | Error msg ->
      Error
        {
          Dev.reason = Dev.Http msg;
          message = Printf.sprintf "device token poll transport failed: %s" msg;
          room_safe_progress = None;
        }
  | Ok (status, resp_body) -> (
      match parse_token_response ~body:resp_body with
      | Ok resp -> Ok resp
      | Error parse_err ->
          if status < 200 || status >= 300 then
            Error
              {
                Dev.reason = Dev.Http parse_err;
                message =
                  Printf.sprintf "device token poll failed (HTTP %d): %s" status
                    parse_err;
                room_safe_progress = None;
              }
          else
            Error
              {
                Dev.reason = Dev.Http parse_err;
                message = parse_err;
                room_safe_progress = None;
              })

(* -------------------------------------------------------------------------- *)
(* poll_once                                                                  *)
(* -------------------------------------------------------------------------- *)

let default_resolve_client_id ~handle =
  Error
    (Printf.sprintf
       "resolve_client_id not provided for handle %s: inject a resolver" handle)

let poll_once ~db ~keys ?http_post ?resolve_client_id ~session_id ~worker_id
    ?(lease_seconds = default_lease_seconds) ?(now = Unix.gettimeofday ()) () =
  let http =
    match http_post with
    | Some f -> f
    | None ->
        fun ~url:_ ~headers:_ ~body:_ ->
          Error
            "http_post not provided: inject a client or wire production HTTP"
  in
  let resolve =
    match resolve_client_id with
    | Some f -> f
    | None -> default_resolve_client_id
  in
  match try_claim ~db ~session_id ~worker_id ~lease_seconds ~now () with
  | Error (Stopped _ as o) -> Ok o
  | Error (Not_due _ as o) -> Ok o
  | Error (Lease_busy _ as o) -> Ok o
  | Error (Authorization_pending _ | Slow_down _ | Granted _) ->
      (* try_claim never returns these. *)
      Error (refuse_storage "internal: unexpected claim outcome")
  | Ok lease -> (
      match Dev.open_secrets ~db ~keys ~id:session_id () with
      | Error e ->
          ignore (clear_lease ~db ~session_id ~token:lease.token ~now);
          Error e
      | Ok (sess, secrets) -> (
          match resolve ~handle:sess.Dev.app.Tx.client_id_handle with
          | Error msg ->
              ignore (clear_lease ~db ~session_id ~token:lease.token ~now);
              Error
                {
                  Dev.reason = Dev.Invalid_input msg;
                  message = msg;
                  room_safe_progress = None;
                }
          | Ok client_id -> (
              let client_id = String.trim client_id in
              if client_id = "" then (
                ignore (clear_lease ~db ~session_id ~token:lease.token ~now);
                Error
                  {
                    Dev.reason = Dev.Invalid_input "empty client_id";
                    message = "resolved client_id is empty";
                    room_safe_progress = None;
                  })
              else
                match
                  request_access_token ~http_post:http
                    ~host:sess.Dev.app.Tx.host ~client_id
                    ~device_code:secrets.Dev.device_code
                with
                | Error e ->
                    (* Transport/parse failure: release lease, back off one
                       interval so restart/retry does not spin. *)
                    let interval = sess.Dev.interval_seconds in
                    let next_poll_at = iso_after ~now ~seconds:interval in
                    ignore
                      (schedule_next ~db ~session_id ~lease_token:lease.token
                         ~interval_seconds:interval ~next_poll_at ~now
                         ~clear_stop:false);
                    Error e
                | Ok response ->
                    apply_token_response ~db ~session_id
                      ~lease_token:lease.token ~response ~now ())))
