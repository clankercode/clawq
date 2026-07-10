(* B774: control-plane HTTP surface for remote subscriber workers.

   Workers connect OUTBOUND to the gateway (no inbound ports on subscriber
   PCs) and authenticate with the gateway auth token. All lease semantics
   live in Work_item_lease; these handlers are thin, and every reply is
   JSON. Prompt and repository data are only ever returned to authenticated
   callers, and provider credentials never appear in any payload. *)

let json_headers =
  Cohttp.Header.of_list [ ("content-type", "application/json") ]

let respond ?(status = `OK) json =
  Cohttp_lwt_unix.Server.respond_string ~status ~headers:json_headers
    ~body:(Yojson.Safe.to_string json)
    ()

let error_response ?(status = `Bad_request) msg =
  respond ~status (`Assoc [ ("error", `String msg) ])

let item_to_json (item : Github_work_item.t) : Yojson.Safe.t =
  let opt f = function Some v -> f v | None -> `Null in
  `Assoc
    [
      ("id", `Int item.id);
      ("dedup_key", `String item.dedup_key);
      ("repo_full_name", `String item.repo_full_name);
      ("is_pr", `Bool item.is_pr);
      ("issue_number", `Int item.issue_number);
      ("requester", `String item.requester);
      ("trigger", `String item.trigger);
      ("runner_pref", opt (fun s -> `String s) item.runner_pref);
      ("host_pref", opt (fun s -> `String s) item.host_pref);
      ("prompt", `String item.prompt);
      ("preamble", `String item.preamble);
      ("status", `String (Github_work_item.string_of_status item.status));
      ("attempt_count", `Int item.attempt_count);
    ]

let member_string key json =
  match Yojson.Safe.Util.member key json with
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let member_int key json =
  match Yojson.Safe.Util.member key json with `Int n -> Some n | _ -> None

let parse_body body_str =
  match Yojson.Safe.from_string body_str with
  | json -> Ok json
  | exception _ ->
      Error "request body must be a JSON object; see docs/llms-full.txt"

let handle_claim ~db body_str =
  match parse_body body_str with
  | Error msg -> error_response msg
  | Ok json -> (
      match
        Work_item_lease.capabilities_of_json
          (Yojson.Safe.Util.member "capabilities" json)
      with
      | Error msg -> error_response msg
      | Ok capabilities -> (
          let lease_seconds =
            match member_int "lease_seconds" json with
            | Some n when n >= 30 && n <= 3600 -> float_of_int n
            | _ -> Work_item_lease.default_lease_seconds
          in
          match
            Work_item_lease.claim ~db ~capabilities ~lease_seconds
              ~now:(Unix.gettimeofday ()) ()
          with
          | None -> respond ~status:`No_content `Null
          | Some lease ->
              respond
                (`Assoc
                   [
                     ("work_item", item_to_json lease.item);
                     ("lease_token", `String lease.token);
                     ("lease_expires_at", `Float lease.expires_at);
                   ])))

let lease_check_response (check : Work_item_lease.lease_check) =
  let name =
    match check with
    | Work_item_lease.Lease_ok -> "ok"
    | Work_item_lease.Lease_stale -> "lease_stale"
    | Work_item_lease.Item_terminal -> "item_terminal"
    | Work_item_lease.Item_missing -> "item_missing"
  in
  error_response ~status:`Conflict
    (Printf.sprintf
       "%s: the lease token no longer owns this item. Stop work on it; release \
        local resources and claim new work."
       name)

let handle_heartbeat ~db body_str =
  match parse_body body_str with
  | Error msg -> error_response msg
  | Ok json -> (
      match (member_int "item_id" json, member_string "lease_token" json) with
      | Some item_id, Some token -> (
          match
            Work_item_lease.heartbeat ~db ~item_id ~token
              ~now:(Unix.gettimeofday ()) ()
          with
          | Ok expires_at ->
              respond (`Assoc [ ("lease_expires_at", `Float expires_at) ])
          | Error check -> lease_check_response check)
      | _ -> error_response "heartbeat requires item_id and lease_token")

let handle_complete ~db ~on_completed body_str =
  match parse_body body_str with
  | Error msg -> error_response msg
  | Ok json -> (
      match (member_int "item_id" json, member_string "lease_token" json) with
      | Some item_id, Some token -> (
          let status =
            Option.bind
              (member_string "status" json)
              Github_work_item.status_of_string
            |> Option.value ~default:Github_work_item.Succeeded
          in
          let result_kind =
            Option.bind
              (member_string "result_kind" json)
              Github_work_item.result_kind_of_string
            |> Option.value ~default:Github_work_item.Reply
          in
          let result_summary =
            Option.value (member_string "result_summary" json) ~default:""
          in
          match
            Work_item_lease.complete ~db ~item_id ~token ~status ~result_kind
              ~result_summary
          with
          | Work_item_lease.Completed ->
              on_completed ~item_id;
              respond (`Assoc [ ("result", `String "completed") ])
          | Work_item_lease.Duplicate_completion ->
              respond (`Assoc [ ("result", `String "duplicate") ])
          | Work_item_lease.Rejected check -> lease_check_response check)
      | _ -> error_response "complete requires item_id and lease_token")

let handle_release ~db body_str =
  match parse_body body_str with
  | Error msg -> error_response msg
  | Ok json -> (
      match (member_int "item_id" json, member_string "lease_token" json) with
      | Some item_id, Some token ->
          if Work_item_lease.release ~db ~item_id ~token then
            respond (`Assoc [ ("result", `String "released") ])
          else
            error_response ~status:`Conflict
              "release refused: token does not own a running lease on this item"
      | _ -> error_response "release requires item_id and lease_token")

let handle_status ~db () =
  let snapshot = Work_item_lease.status_snapshot ~db in
  respond
    (`Assoc
       [
         ("queued", `Int snapshot.queued);
         ("running", `Int snapshot.running);
         ("blocked", `Int snapshot.blocked);
         ( "workers",
           `List
             (List.map
                (fun (w : Work_item_lease.worker_row) ->
                  `Assoc
                    [
                      ("worker_id", `String w.row_worker_id);
                      ("last_seen_at", `Float w.last_seen_at);
                      ( "capabilities",
                        match w.row_capabilities with
                        | Some c -> Work_item_lease.capabilities_to_json c
                        | None -> `Null );
                    ])
                snapshot.workers) );
         ( "leases",
           `List
             (List.map
                (fun (id, owner, expires) ->
                  `Assoc
                    [
                      ("item_id", `Int id);
                      ("owner", `String owner);
                      ("expires_at", `Float expires);
                    ])
                snapshot.leases) );
       ])

(** Route a /worker/* request. Returns None when the path is not ours. *)
let handle ~db ?(on_completed = fun ~item_id:_ -> ()) ~meth ~path ~body_str () =
  Work_item_lease.init_schema db;
  match (meth, path) with
  | `POST, "/worker/claim" -> Some (handle_claim ~db body_str)
  | `POST, "/worker/heartbeat" -> Some (handle_heartbeat ~db body_str)
  | `POST, "/worker/complete" ->
      Some (handle_complete ~db ~on_completed body_str)
  | `POST, "/worker/release" -> Some (handle_release ~db body_str)
  | `GET, "/worker/status" -> Some (handle_status ~db ())
  | _ -> None
