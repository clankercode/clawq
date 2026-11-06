(* The only way standing-order code calls Slack: a closed method list, a
   persisted Standing_order snapshot, the current config's credentials and
   channel allowlist, an audited egress check per call, and typed errors. No
   call is made under an unpersisted snapshot. Peer lookup shares this
   audited transport through a users.info-only scope that also admits DMs. *)
open Lwt.Syntax

type http =
  verb:string ->
  token:string ->
  method_:string ->
  params:(string * Yojson.Safe.t) list ->
  (int * string) Lwt.t

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type http_response =
  verb:string ->
  token:string ->
  method_:string ->
  params:(string * Yojson.Safe.t) list ->
  response Lwt.t

let configured_limiter : Slack_read_limiter.t option ref = ref None
let configure_limiter limiter = configured_limiter := limiter

let ensure_limiter () =
  match !configured_limiter with
  | Some limiter -> limiter
  | None ->
      let limiter = Slack_read_limiter.create () in
      configure_limiter (Some limiter);
      limiter

(* Explicit fixtures can opt out while every production factory gets the
   process dependency, including management sends without a worker context. *)
let choose_limiter ?limiter ~fixture () =
  match (limiter, !configured_limiter) with
  | Some limiter, _ | None, Some limiter -> Some limiter
  | None, None when fixture || Lwt.get Slack_transport.key <> None -> None
  | None, None -> Some (ensure_limiter ())

let retry_after_seconds headers =
  let values =
    List.filter_map
      (fun (name, value) ->
        if String.lowercase_ascii name = "retry-after" then
          match float_of_string_opt (String.trim value) with
          | Some n when Float.is_finite n && n >= 0. -> Some n
          | _ -> None
        else None)
      headers
  in
  match values with [] -> None | n :: rest -> Some (List.fold_left max n rest)

let retry_delay headers =
  Option.value (retry_after_seconds headers) ~default:60.

(* Identity comes from a persisted order, or the verified setup route before
   pending insertion. Unknown peer scope shares a conservative process bucket. *)
let installation ~db ~(snapshot : Access_snapshot.t) ?team_id ?app_id () =
  match (team_id, app_id) with
  | Some team, Some app -> team ^ ":" ^ app
  | _ ->
      let identity =
        Option.bind snapshot.standing_order (fun order ->
            try
              let stmt =
                Sqlite3.prepare db
                  "SELECT team_id,api_app_id FROM slack_standing_orders WHERE \
                   id=?"
              in
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  ignore
                    (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT order.order_id));
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.ROW ->
                      Some
                        (Sql_util.text_column stmt 0
                        ^ ":"
                        ^ Sql_util.text_column stmt 1)
                  | _ -> None)
            with _ -> None)
      in
      Option.value identity ~default:"unbound"

let method_key ~installation method_ = installation ^ ":" ^ method_

let produces_message = function
  | "chat.postMessage" | "files.completeUploadExternal" -> true
  | _ -> false

type error =
  | Rate_limited
  | Http_status of int
  | Transport
  | Audit_unavailable
  | Snapshot_unavailable
  | Slack_error of string  (** Slack's ["error"] code only, never the body. *)
  | Egress_denied of string
  | Method_not_permitted of string
  | Channel_denied
  | Not_configured
  | Dispatch_refused

type call =
  string -> (string * Yojson.Safe.t) list -> (Yojson.Safe.t, error) result Lwt.t

(* Method -> verb, one entry per call site; the egress table in the docs is
   derived from this list. *)
let methods =
  [
    ("auth.test", "POST");
    ("bots.info", "GET");
    ("users.info", "GET");
    ("users.list", "GET");
    ("conversations.info", "GET");
    ("conversations.members", "GET");
    ("chat.postMessage", "POST");
    ("chat.update", "POST");
    ("conversations.history", "GET");
    ("conversations.replies", "GET");
    ("files.info", "GET");
    ("canvases.getContent", "POST");
    ("reactions.get", "GET");
    ("files.getUploadURLExternal", "GET");
    ("files.completeUploadExternal", "POST");
    ("files.delete", "POST");
  ]

(* Raw capability uploads are audited operations, never Web API methods. *)
let operations = "upload" :: List.map fst methods
let api_uri method_ = "https://slack.com/api/" ^ method_
let upload_prefix = "https://files.slack.com/"
let auth_headers token = [ ("Authorization", "Bearer " ^ token) ]

let query_value = function
  | `String s -> s
  | `Int n -> string_of_int n
  | `Bool b -> string_of_bool b
  | json -> Yojson.Safe.to_string json

(* Http_client's optional HAR capture includes bodies and returned upload
   capabilities. Standing orders must never enter that path. Keep its timeout
   and user-agent conventions, with a single socket request and no capture. *)
let perform request =
  Http_client.labeled_timeout ~label:"standing_order_slack"
    !Http_client.default_timeout_s (fun () ->
      let* response, stream = request () in
      let+ body = Cohttp_lwt.Body.to_string stream in
      {
        status = Cohttp.Response.status response |> Cohttp.Code.code_of_status;
        headers = Cohttp.Response.headers response |> Cohttp.Header.to_list;
        body;
      })

let headers values =
  Http_client.ensure_user_agent values |> Cohttp.Header.of_list

let api_get ~uri ~headers:values =
  perform (fun () ->
      Cohttp_lwt_unix.Client.get ~headers:(headers values) (Uri.of_string uri))

let api_post ~uri ~headers:values ~body =
  perform (fun () ->
      Cohttp_lwt_unix.Client.post
        ~headers:(headers (("Content-Type", "application/json") :: values))
        ~body:(Cohttp_lwt.Body.of_string body)
        (Uri.of_string uri))

let api_transport : Slack_transport.t =
  {
    get =
      (fun ~uri ~headers ->
        let+ r = api_get ~uri ~headers in
        (r.status, r.body));
    post =
      (fun ~uri ~headers ~body ->
        let+ r = api_post ~uri ~headers ~body in
        (r.status, r.body));
  }

let adapt_http (http : http) : http_response =
 fun ~verb ~token ~method_ ~params ->
  let+ status, body = http ~verb ~token ~method_ ~params in
  { status; headers = []; body }

let default_http_response : http_response =
 fun ~verb ~token ~method_ ~params ->
  let uri =
    match verb with
    | "GET" ->
        Uri.make ~scheme:"https" ~host:"slack.com" ~path:("/api/" ^ method_)
          ~query:(List.map (fun (k, v) -> (k, [ query_value v ])) params)
          ()
        |> Uri.to_string
    | _ -> api_uri method_
  in
  let body = Yojson.Safe.to_string (`Assoc params) in
  match Lwt.get Slack_transport.key with
  | Some transport ->
      let* status, body =
        match verb with
        | "GET" -> transport.get ~uri ~headers:(auth_headers token)
        | "POST" -> transport.post ~uri ~headers:(auth_headers token) ~body
        | _ -> Lwt.fail_with "unsupported Slack verb"
      in
      Lwt.return { status; headers = []; body }
  | None -> (
      match verb with
      | "GET" -> api_get ~uri ~headers:(auth_headers token)
      | "POST" -> api_post ~uri ~headers:(auth_headers token) ~body
      | _ -> Lwt.fail_with "unsupported Slack verb")

let default_http : http =
 fun ~verb ~token ~method_ ~params ->
  let+ r = default_http_response ~verb ~token ~method_ ~params in
  (r.status, r.body)

let access_key ~channel_id = "slack:" ^ channel_id

let audit_label ~order_id ~unit_label =
  Printf.sprintf "standing_order:%s:%s"
    (Option.value order_id ~default:"none")
    unit_label

(* Decision 15: unlike [Access_snapshot.create_and_persist], a row that can't
   be written is an error, so no egress audit row points at a missing
   snapshot. *)
let snapshot_for ~work_type ~db ~config ~channel_id ~order_ref =
  match
    let snap =
      Access_snapshot.create ~config ~work_type
        ~session_key:(access_key ~channel_id) ~room_id:channel_id
        ?standing_order:order_ref ()
    in
    Access_snapshot.persist ~db snap;
    snap
  with
  | snap -> Ok snap
  | exception _ -> Error "snapshot_not_persisted"

let snapshot = snapshot_for ~work_type:Standing_order

let peer_lookup_snapshot ~db ~config ~channel_id =
  snapshot_for ~work_type:Room_turn ~db ~config ~channel_id ~order_ref:None

(* Slack codes are short snake_case words; anything else is not echoed. *)
let slack_code = function
  | `String s
    when String.length s > 0
         && String.length s <= 64
         && String.for_all
              (function
                | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true | _ -> false)
              s ->
      s
  | _ -> "unknown"

let error_code = function
  | Rate_limited -> "rate_limited"
  | Http_status n -> "http_" ^ string_of_int n
  | Transport -> "transport"
  | Audit_unavailable -> "audit_unavailable"
  | Snapshot_unavailable -> "snapshot_unavailable"
  | Slack_error code -> "slack:" ^ code
  | Egress_denied m -> "egress_denied:" ^ m
  | Method_not_permitted m -> "method_not_permitted:" ^ m
  | Channel_denied -> "channel_denied"
  | Not_configured -> "not_configured"
  | Dispatch_refused -> "dispatch_refused"

let decode status body =
  if status = 429 then Error Rate_limited
  else if status <> 200 then Error (Http_status status)
  else
    match Yojson.Safe.from_string body with
    | exception _ -> Error Transport
    | `Assoc fields as json -> (
        match List.assoc_opt "ok" fields with
        | Some (`Bool true) -> Ok json
        | _ -> (
            match
              slack_code
                (Option.value (List.assoc_opt "error" fields) ~default:`Null)
            with
            | "ratelimited" | "rate_limited" -> Error Rate_limited
            | code -> Error (Slack_error code)))
    | _ -> Error Transport

(* Steps 2-4: current credentials, channel allowlist, then the audited egress
   check against the current config's rules for this channel. *)
type scope = Standing_order | Peer_lookup

let persisted_snapshot ~scope ~db ~(snapshot : Access_snapshot.t) ~channel_id =
  try
    let work_type =
      match scope with
      | Standing_order -> Access_snapshot.Standing_order
      | Peer_lookup -> Access_snapshot.Room_turn
    in
    snapshot.work_type = work_type
    && snapshot.session_key = Some (access_key ~channel_id)
    && snapshot.room_id = Some channel_id
    && Access_snapshot.get_by_id ~db snapshot.id = Some snapshot
  with _ -> false

let capability_names =
  [ "slack_huddles"; "slack_huddle_content"; "slack_standing_order_fire" ]

let capability_denied (cfg : Runtime_config.t) ~channel_id =
  let access =
    Runtime_config.resolve_effective_access cfg
      ~session_key:(access_key ~channel_id) ()
  in
  let values =
    List.map (fun (i : Runtime_config.effective_access_item) -> i.value)
  in
  List.find_opt
    (fun name ->
      (not cfg.security.tools_enabled)
      ||
      match
        Tool_authz.decide ~equivalence_names:[ name ]
          ~allowed_tools:(values access.allowed_tools)
          ~denied_tools:(values access.denied_tools)
          ()
      with
      | Tool_authz.Allowed -> false
      | Tool_authz.Denied _ -> true)
    capability_names

let preflight ?uri ?(sensitive_uri = false) ?(capability_fence = false) ~scope
    ~db ~(cfg : Runtime_config.t) ~(snapshot : Access_snapshot.t) ~channel_id
    ~audit method_ verb =
  try
    match cfg.channels.slack with
    | _
      when capability_fence && scope = Standing_order
           && capability_denied cfg ~channel_id <> None ->
        Error Dispatch_refused
    | Some sc when Runtime_config.slack_has_valid_credentials sc -> (
        if
          not
            (Slack_config_types.valid_id
               (match scope with
               | Standing_order -> "CG"
               | Peer_lookup -> "CGD")
               channel_id
            && Slack_api.is_allowed_allowlist ~kind:"channel" ~id:channel_id
                 sc.allow_channels)
        then Error Channel_denied
        else if not (persisted_snapshot ~scope ~db ~snapshot ~channel_id) then
          Error Snapshot_unavailable
        else
          let key = access_key ~channel_id in
          let access =
            Runtime_config.resolve_effective_access cfg ~session_key:key ()
          in
          let rules =
            Runtime_config.effective_egress_rules cfg access.egress_rules
          in
          let audit : Policy_http_client.audit_context =
            {
              db = Some db;
              session_key = Some key;
              snapshot_id = Some snapshot.id;
              tool_name = Some audit;
              profile_id = snapshot.profile_id;
              credential_handle_ids = [];
            }
          in
          match
            Policy_http_client.check_policy ~rules
              ~uri:(Option.value uri ~default:(api_uri method_))
              ~method_:verb ~audit ~strict_audit:true ~sensitive_uri ()
          with
          | Ok () -> Ok (String.trim sc.bot_token)
          | Error _ -> Error (Egress_denied method_)
          | exception _ -> Error Audit_unavailable)
    | _ -> Error Not_configured
  with _ -> Error Transport

(* Only the API's own selector may carry the bound channel. Check every
   occurrence, including duplicate JSON keys; never let another selector
   bypass the scope. Upload completion must create a top-level message. *)
let channel_params_allowed ~channel_id method_ params =
  let selector =
    match method_ with
    | "conversations.info" | "conversations.members" | "conversations.history"
    | "conversations.replies" | "chat.postMessage" | "chat.update"
    | "reactions.get" ->
        Some "channel"
    | "files.completeUploadExternal" -> Some "channel_id"
    | _ -> None
  in
  List.for_all
    (fun (key, value) ->
      match key with
      | "channel" | "channel_id" | "channels" | "conversation"
      | "conversation_id" ->
          selector = Some key && value = `String channel_id
      | "thread_ts" when method_ = "files.completeUploadExternal" -> false
      | _ -> true)
    params

let make_scoped ~scope ~db ~current_config ~snapshot ~channel_id ~audit ?http
    ?http_response ?limiter ?team_id ?app_id
    ?(priority = Slack_read_limiter.Live) ?(limit = fun () -> Lwt.return_unit)
    ?(before_send = fun () -> Ok ()) ?(capability_fence = false) () : call =
  let http =
    match (http, http_response) with
    | Some _, Some _ ->
        invalid_arg "standing-order HTTP fixtures are mutually exclusive"
    | Some http, None -> adapt_http http
    | None, Some http -> http
    | None, None -> default_http_response
  in
  let limiter =
    choose_limiter ?limiter ~fixture:(http != default_http_response) ()
  in
  let installation = installation ~db ~snapshot ?team_id ?app_id () in
  fun method_ params ->
    let permitted =
      match scope with
      | Standing_order -> methods
      | Peer_lookup -> [ ("users.info", "GET") ]
    in
    match List.assoc_opt method_ permitted with
    | None -> Lwt.return (Error (Method_not_permitted method_))
    | Some _ when not (channel_params_allowed ~channel_id method_ params) ->
        Lwt.return (Error Channel_denied)
    | Some verb ->
        Lwt.catch
          (fun () ->
            let* () = limit () in
            let key = method_key ~installation method_ in
            let* () =
              match limiter with
              | None -> Lwt.return_unit
              | Some limiter ->
                  Slack_read_limiter.before_call limiter ~key ~priority
                    ?channel:
                      (if produces_message method_ then Some channel_id
                       else None)
                    ()
            in
            match
              preflight ~scope ~capability_fence ~db ~cfg:(current_config ())
                ~snapshot ~channel_id ~audit method_ verb
            with
            | Error _ as denied -> Lwt.return denied
            | Ok token -> (
                match before_send () with
                | Error error -> Lwt.return (Error error)
                | Ok () ->
                    let* response = http ~verb ~token ~method_ ~params in
                    let result = decode response.status response.body in
                    (match (result, limiter) with
                    | Error Rate_limited, Some limiter ->
                        Slack_read_limiter.note_retry_after limiter ~key
                          (if response.status = 429 then
                             retry_delay response.headers
                           else 60.)
                    | _ -> ());
                    Lwt.return result))
          (function
            | Lwt.Canceled -> Lwt.fail Lwt.Canceled
            | _ ->
                Logs.debug (fun m -> m "standing-order Slack transport failed");
                Lwt.return (Error Transport))

let make = make_scoped ~scope:Standing_order
let make_peer_lookup = make_scoped ~scope:Peer_lookup
