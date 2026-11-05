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
let api_transport : Slack_transport.t =
  let perform request =
    Http_client.labeled_timeout ~label:"standing_order_slack"
      !Http_client.default_timeout_s (fun () ->
        let* response, stream = request () in
        let+ body = Cohttp_lwt.Body.to_string stream in
        (Cohttp.Response.status response |> Cohttp.Code.code_of_status, body))
  in
  let headers values =
    Http_client.ensure_user_agent values |> Cohttp.Header.of_list
  in
  {
    get =
      (fun ~uri ~headers:values ->
        perform (fun () ->
            Cohttp_lwt_unix.Client.get ~headers:(headers values)
              (Uri.of_string uri)));
    post =
      (fun ~uri ~headers:values ~body ->
        perform (fun () ->
            Cohttp_lwt_unix.Client.post
              ~headers:
                (headers (("Content-Type", "application/json") :: values))
              ~body:(Cohttp_lwt.Body.of_string body)
              (Uri.of_string uri)));
  }

let default_http : http =
 fun ~verb ~token ~method_ ~params ->
  (* Preserve Slack_transport's Lwt-local fixture injection. *)
  let transport =
    Option.value (Lwt.get Slack_transport.key) ~default:api_transport
  in
  match verb with
  | "GET" ->
      let query = List.map (fun (k, v) -> (k, [ query_value v ])) params in
      let uri =
        Uri.make ~scheme:"https" ~host:"slack.com" ~path:("/api/" ^ method_)
          ~query ()
        |> Uri.to_string
      in
      transport.get ~uri ~headers:(auth_headers token)
  | "POST" ->
      transport.post ~uri:(api_uri method_) ~headers:(auth_headers token)
        ~body:(Yojson.Safe.to_string (`Assoc params))
  | other -> Lwt.fail_with ("unsupported Slack verb " ^ other)

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

let make_scoped ~scope ~db ~current_config ~snapshot ~channel_id ~audit
    ?(http = default_http) ?(limit = fun () -> Lwt.return_unit)
    ?(before_send = fun () -> Ok ()) ?(capability_fence = false) () : call =
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
          match
            preflight ~scope ~capability_fence ~db ~cfg:(current_config ())
              ~snapshot ~channel_id ~audit method_ verb
          with
          | Error _ as denied -> Lwt.return denied
          | Ok token -> (
              match before_send () with
              | Error error -> Lwt.return (Error error)
              | Ok () ->
                  let* status, body = http ~verb ~token ~method_ ~params in
                  Lwt.return (decode status body)))
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | _ ->
              Logs.debug (fun m -> m "standing-order Slack transport failed");
              Lwt.return (Error Transport))

let make = make_scoped ~scope:Standing_order
let make_peer_lookup = make_scoped ~scope:Peer_lookup
