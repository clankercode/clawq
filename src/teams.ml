(* MS Teams channel via Microsoft Bot Framework webhook *)

include Teams_auth
include Teams_file_consent

let max_message_chars = 28672
let dedup = Channel_util.Lru_dedup.create 500

let session_key ~team_id ~conversation_id =
  Printf.sprintf "teams:%s:%s" team_id conversation_id

(** Generate a thread-aware session key. *)
let thread_session_key ~team_id ~conversation_id ~reply_to_id =
  if reply_to_id = "" then session_key ~team_id ~conversation_id
  else
    Printf.sprintf "teams:%s:%s:thread:%s" team_id conversation_id reply_to_id

let incoming_rate_limited_message =
  "Please slow down, I can only process a limited number of messages per \
   minute."

type rate_limit_decision = Allowed | Rate_limited of { should_warn : bool }

let rate_limit_warnings : (string, float) Hashtbl.t = Hashtbl.create 16

let check_incoming_rate_limit ?event_limiter ~limiter_key () =
  let open Lwt.Syntax in
  let* rate_ok =
    match event_limiter with
    | Some lim -> Rate_limiter.check_and_consume lim ~key:limiter_key
    | None -> Lwt.return true
  in
  if rate_ok then Lwt.return Allowed
  else
    let now = Unix.gettimeofday () in
    let should_warn =
      match Hashtbl.find_opt rate_limit_warnings limiter_key with
      | Some last -> now -. last >= 60.0
      | None -> true
    in
    if should_warn then Hashtbl.replace rate_limit_warnings limiter_key now;
    Lwt.return (Rate_limited { should_warn })

let user_facing_error_of_exn = function
  | Failure msg -> msg
  | exn -> Printexc.to_string exn

let agent_error_message err =
  Printf.sprintf "Sorry, an error occurred processing your message: %s" err

(** Resolve the session key for a Teams conversation. *)
let room_has_profile_binding ~(session_manager : Session.t) ~conversation_id =
  match Session.get_db session_manager with
  | Some db -> (
      match Memory.get_room_profile_binding ~db ~room_id:conversation_id with
      | Some _ -> true
      | None -> false)
  | None -> false

let resolve_session_key ~(session_manager : Session.t) ~team_id ~conversation_id
    ?(reply_to_id = "") () =
  if room_has_profile_binding ~session_manager ~conversation_id then
    "teams:" ^ Session.sanitize_session_key conversation_id
  else thread_session_key ~team_id ~conversation_id ~reply_to_id

let record_scoped_room_history_if_bound ~(session_manager : Session.t) ~team_id
    ~conversation_id ~user_id ~user_name ~text =
  let cfg = Session.get_config session_manager in
  if
    String.trim text <> ""
    && room_has_profile_binding ~session_manager ~conversation_id
    && Connector_capabilities.should_capture_history
         ~enabled:cfg.connector_history.enabled Connector_capabilities.teams
  then
    let key =
      resolve_session_key ~session_manager ~team_id ~conversation_id ()
    in
    let db =
      if cfg.connector_history.persist_to_db then Session.get_db session_manager
      else None
    in
    Connector_history.record ?db ~persist:cfg.connector_history.persist_to_db
      ~key ~room_id:conversation_id ~connector_type:"teams"
      ~channel_type:"teams" ~max:cfg.connector_history.max_messages
      ~sender_name:user_name ~sender_id:user_id ~text ()

let consent_room_context ~(session_manager : Session.t) ~conversation_id
    ~user_group ?access_snapshot_id () =
  match Session.get_db session_manager with
  | None -> None
  | Some db -> (
      match Memory.get_room_profile_for_room ~db ~room_id:conversation_id with
      | None -> None
      | Some profile ->
          Some
            {
              room_id = conversation_id;
              session_key =
                "teams:" ^ Session.sanitize_session_key conversation_id;
              profile_name = profile.name;
              user_group;
              access_snapshot_id;
            })

let slash_command_name token =
  let token = String.trim token in
  let token =
    if String.length token > 0 && token.[0] = '/' then
      String.sub token 1 (String.length token - 1)
    else token
  in
  match String.index_opt token '@' with
  | Some idx -> String.sub token 0 idx
  | None -> token

let known_slash_subcommand name =
  match Slash_commands.handle ("/" ^ name) with
  | Slash_commands.NotACommand -> false
  | _ -> true

let normalize_clawq_slash_text text =
  let trimmed = String.trim text in
  let parts =
    String.split_on_char ' ' trimmed |> List.filter (fun part -> part <> "")
  in
  match parts with
  | first :: rest
    when String.lowercase_ascii (slash_command_name first) = "clawq" -> (
      match rest with
      | [] -> "/help"
      | subcommand :: _ ->
          if known_slash_subcommand (slash_command_name subcommand) then
            "/" ^ String.concat " " rest
          else "/help")
  | _ -> trimmed

let encode_channel_id ~service_url ~conversation_id =
  service_url ^ "|" ^ conversation_id

let decode_channel_id channel_id =
  match String.split_on_char '|' channel_id with
  | service_url :: rest -> (service_url, String.concat "|" rest)
  | [] -> ("", channel_id)

type mention = { mention_id : string; mention_name : string }

let dedup_seen id =
  if id = "" then false else Channel_util.Lru_dedup.check_and_mark dedup id

(** [dedup_seen_persistent ~db ~conversation_id ~activity_id] checks if an
    activity has been processed before using persistent storage. *)
let dedup_seen_persistent ~db ~conversation_id ~activity_id =
  if activity_id = "" then false
  else
    match db with
    | Some db ->
        Memory.teams_dedup_check_and_mark ~db ~conversation_id ~activity_id
    | None -> dedup_seen activity_id

(* --- Teams Bot Framework outbound rate limiting --- *)

let conv_last_request : (string, float) Hashtbl.t = Hashtbl.create 32

let throttle_for_conversation ~conversation_id =
  let now = Unix.gettimeofday () in
  let min_interval = 1.0 in
  let last =
    Option.value ~default:0.0
      (Hashtbl.find_opt conv_last_request conversation_id)
  in
  let target = Float.max now (last +. min_interval) in
  let wait = target -. now in
  Hashtbl.replace conv_last_request conversation_id target;
  if wait > 0.001 then Lwt_unix.sleep wait else Lwt.return_unit

let is_retryable_status status =
  status = 429 || status = 412 || status = 502 || status = 504

let with_retry ?(max_retries = 3) ~conversation_id ~f () =
  let open Lwt.Syntax in
  let rec loop attempt =
    let* () = throttle_for_conversation ~conversation_id in
    let* status, result = f () in
    if is_retryable_status status && attempt < max_retries then begin
      let delay =
        Float.min 10.0 (Float.max 1.0 (Float.pow 2.0 (float_of_int attempt)))
      in
      Logs.warn (fun m ->
          m "Teams: HTTP %d on conv=%s, retrying in %.1fs (attempt %d/%d)"
            status conversation_id delay (attempt + 1) max_retries);
      let* () = Lwt_unix.sleep delay in
      loop (attempt + 1)
    end
    else Lwt.return (status, result)
  in
  loop 0

let post_json_throttled ~conversation_id ~uri ~headers ~body =
  with_retry ~conversation_id
    ~f:(fun () -> Http_client.post_json ~uri ~headers ~body)
    ()

let put_json_throttled ~conversation_id ~uri ~headers ~body =
  with_retry ~conversation_id
    ~f:(fun () -> Http_client.put_json ~uri ~headers ~body)
    ()

let delete_throttled ~conversation_id ~uri ~headers =
  with_retry ~conversation_id
    ~f:(fun () -> Http_client.delete ~uri ~headers ~body:"")
    ()

(* Send a typing indicator via Bot Framework REST API. *)
let send_typing_activity ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None -> Lwt.return_unit
  | Some token ->
      let uri =
        Printf.sprintf "%s/v3/conversations/%s/activities"
          (String.trim service_url)
          (Uri.pct_encode conversation_id)
      in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let body =
        `Assoc [ ("type", `String "typing") ] |> Yojson.Safe.to_string
      in
      let* () = throttle_for_conversation ~conversation_id in
      let* status, _resp = Http_client.post_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then
        Logs.debug (fun m ->
            m "Teams: typing indicator failed (HTTP %d) conv=%s" status
              conversation_id);
      Lwt.return_unit

(* Split text into chunks of at most max_message_chars, breaking at whitespace *)
let split_message text =
  if String.length text <= max_message_chars then [ text ]
  else begin
    let result = ref [] in
    let len = String.length text in
    let pos = ref 0 in
    while !pos < len do
      let remaining = len - !pos in
      if remaining <= max_message_chars then begin
        result := String.sub text !pos remaining :: !result;
        pos := len
      end
      else begin
        (* Find last whitespace before the limit *)
        let limit = !pos + max_message_chars in
        let cut = ref limit in
        while !cut > !pos && text.[!cut] <> ' ' && text.[!cut] <> '\n' do
          decr cut
        done;
        if !cut = !pos then begin
          (* No whitespace before the limit: forced mid-word break. The char
             at [limit] is not whitespace, so do not skip past it or it would
             be dropped from the output. *)
          result := String.sub text !pos max_message_chars :: !result;
          pos := limit
        end
        else begin
          (* Break at whitespace, consuming the whitespace char as separator. *)
          result := String.sub text !pos (!cut - !pos) :: !result;
          pos := !cut + 1
        end
      end
    done;
    List.rev !result
  end

(* Build a reply JSON body with optional @mention and notification alert.
   mention_mode: "entity" (default), "text" (@Name), "none". *)
let build_reply_body ~alert ~text ~mention ~mention_mode =
  let text_with_mention, entities =
    match mention with
    | None -> (text, [])
    | Some { mention_id; mention_name } -> (
        match mention_mode with
        | "none" -> (text, [])
        | "text" ->
            let at_tag = Printf.sprintf "@%s" mention_name in
            (Printf.sprintf "%s %s" at_tag text, [])
        | _ ->
            (* "entity" mode: proper Teams mention with entity markup *)
            let at_tag = Printf.sprintf "<at>%s</at>" mention_name in
            let full_text = Printf.sprintf "%s %s" at_tag text in
            let entity =
              `Assoc
                [
                  ("type", `String "mention");
                  ( "mentioned",
                    `Assoc
                      [
                        ("id", `String mention_id);
                        ("name", `String mention_name);
                      ] );
                  ("text", `String at_tag);
                ]
            in
            (full_text, [ entity ]))
  in
  let text_with_mention = Markdown_util.normalize_tables text_with_mention in
  let base =
    [
      ("type", `String "message");
      ("textFormat", `String "markdown");
      ("text", `String text_with_mention);
    ]
  in
  let base =
    base
    @ [
        ( "channelData",
          `Assoc [ ("notification", `Assoc [ ("alert", `Bool alert) ]) ] );
      ]
  in
  let fields =
    match entities with
    | [] -> base
    | ents -> base @ [ ("entities", `List ents) ]
  in
  `Assoc fields |> Yojson.Safe.to_string

(* Build Bot Framework REST API URI: empty reply_to_id = new message, else threaded reply. *)
let build_reply_uri ~service_url ~conversation_id ~reply_to_id =
  if reply_to_id = "" then
    Printf.sprintf "%s/v3/conversations/%s/activities" (String.trim service_url)
      (Uri.pct_encode conversation_id)
  else
    Printf.sprintf "%s/v3/conversations/%s/activities/%s"
      (String.trim service_url)
      (Uri.pct_encode conversation_id)
      (Uri.pct_encode reply_to_id)

(* Send a reply via Bot Framework REST API.
   ~alert controls channelData.notification.alert: true triggers a
   desktop/mobile notification toast, false suppresses it. *)
let send_reply ?(alert = false) ~(config : Runtime_config.teams_config)
    ~service_url ~conversation_id ~reply_to_id ~text ?mention () =
  let open Lwt.Syntax in
  (* B464: guard against empty payloads — Teams rejects empty text with HTTP 400. *)
  if String.trim text = "" then begin
    Logs.warn (fun m ->
        m
          "Teams: refusing to send empty reply (conv=%s reply_to_id=%s) — \
           caller passed empty/whitespace text; Teams would reject with HTTP \
           400 BadSyntax"
          conversation_id reply_to_id);
    Lwt.return ""
  end
  else if
    service_url = ""
    || not
         (String.length service_url >= 8
         && (String.sub service_url 0 8 = "https://"
            || String.sub service_url 0 7 = "http://"))
  then begin
    Logs.err (fun m ->
        m
          "Teams: service_url is empty or missing scheme (service_url=%S, \
           conversation_id=%S); cannot send reply"
          service_url conversation_id);
    Lwt.return ""
  end
  else
    let* token_opt = fetch_token ~config in
    match token_opt with
    | None ->
        Logs.err (fun m -> m "Teams: cannot send reply, no OAuth token");
        Lwt.return ""
    | Some token ->
        let chunks = split_message text in
        let last_id = ref "" in
        let* () =
          Lwt_list.iter_s
            (fun chunk ->
              let uri =
                build_reply_uri ~service_url ~conversation_id ~reply_to_id
              in
              let headers = [ ("Authorization", "Bearer " ^ token) ] in
              let body =
                build_reply_body ~alert ~text:chunk ~mention
                  ~mention_mode:config.mention_mode
              in
              Logs.info (fun m ->
                  m "Teams: POST %s body_len=%d" uri (String.length body));
              let* status, resp =
                post_json_throttled ~conversation_id ~uri ~headers ~body
              in
              if status >= 200 && status < 300 then begin
                Logs.info (fun m ->
                    m "Teams: POST ok (HTTP %d) conv=%s resp_len=%d" status
                      conversation_id (String.length resp));
                try
                  let json = Yojson.Safe.from_string resp in
                  let open Yojson.Safe.Util in
                  let id =
                    try json |> member "id" |> to_string with _ -> ""
                  in
                  if id <> "" then last_id := id
                with _ -> ()
              end
              else
                Logs.warn (fun m ->
                    m "Teams: send_reply failed (HTTP %d) conv=%s: %s" status
                      conversation_id resp);
              Lwt.return_unit)
            chunks
        in
        Lwt.return !last_id

let edit_activity ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~activity_id ~text () =
  let open Lwt.Syntax in
  (* B464: same guard as send_reply — Teams rejects empty text. *)
  if String.trim text = "" then begin
    Logs.warn (fun m ->
        m
          "Teams: refusing to edit activity %s with empty text (conv=%s); \
           Teams would reject with HTTP 400 BadSyntax"
          activity_id conversation_id);
    Lwt.fail (Failure "Teams edit_activity: empty text")
  end
  else
    let* token_opt = fetch_token ~config in
    match token_opt with
    | None ->
        Logs.err (fun m -> m "Teams: cannot edit activity, no OAuth token");
        Lwt.fail (Failure "Teams edit_activity: no OAuth token")
    | Some token ->
        let uri =
          Printf.sprintf "%s/v3/conversations/%s/activities/%s"
            (String.trim service_url)
            (Uri.pct_encode conversation_id)
            (Uri.pct_encode activity_id)
        in
        let headers = [ ("Authorization", "Bearer " ^ token) ] in
        let body =
          `Assoc
            [
              ("type", `String "message");
              ("textFormat", `String "markdown");
              ("text", `String text);
            ]
          |> Yojson.Safe.to_string
        in
        let* status, resp =
          put_json_throttled ~conversation_id ~uri ~headers ~body
        in
        if status < 200 || status >= 300 then begin
          Logs.warn (fun m ->
              m "Teams: edit_activity failed (HTTP %d) conv=%s activity=%s: %s"
                status conversation_id activity_id resp);
          Lwt.fail
            (Failure
               (Printf.sprintf "Teams edit_activity HTTP %d: %s" status resp))
        end
        else Lwt.return_unit

let delete_activity ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~activity_id () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot delete activity, no OAuth token");
      Lwt.return_unit
  | Some token ->
      let uri =
        Printf.sprintf "%s/v3/conversations/%s/activities/%s"
          (String.trim service_url)
          (Uri.pct_encode conversation_id)
          (Uri.pct_encode activity_id)
      in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let* status, resp = delete_throttled ~conversation_id ~uri ~headers in
      if status < 200 || status >= 300 then
        Logs.warn (fun m ->
            m "Teams: delete_activity failed (HTTP %d) conv=%s activity=%s: %s"
              status conversation_id activity_id resp);
      Lwt.return_unit

let send_adaptive_card ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~reply_to_id ~card () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot send adaptive card, no OAuth token");
      Lwt.return ""
  | Some token ->
      let uri = build_reply_uri ~service_url ~conversation_id ~reply_to_id in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let body = Yojson.Safe.to_string card in
      let* status, resp =
        post_json_throttled ~conversation_id ~uri ~headers ~body
      in
      if status >= 200 && status < 300 then begin
        Logs.info (fun m ->
            m "Teams: send_adaptive_card ok (HTTP %d) conv=%s resp_len=%d"
              status conversation_id (String.length resp));
        try
          let json = Yojson.Safe.from_string resp in
          let open Yojson.Safe.Util in
          let id = try json |> member "id" |> to_string with _ -> "" in
          Lwt.return id
        with _ -> Lwt.return ""
      end
      else begin
        Logs.warn (fun m ->
            m "Teams: send_adaptive_card failed (HTTP %d) conv=%s: %s" status
              conversation_id resp);
        Lwt.return ""
      end

(* File Consent Card flow wrappers — bind fetch_token and post_json_throttled *)
let send_file_consent_card ?room_context ~(config : Runtime_config.teams_config)
    ~service_url ~conversation_id ~reply_to_id ~filename ~description
    ~size_bytes ~consent_id () =
  Teams_file_consent.send_file_consent_card ?room_context ~fetch_token
    ~post_json_throttled ~config ~service_url ~conversation_id ~reply_to_id
    ~filename ~description ~size_bytes ~consent_id ()

let send_file_info_card ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~filename ~content_url ~unique_id ~file_type () =
  Teams_file_consent.send_file_info_card ~fetch_token ~post_json_throttled
    ~config ~service_url ~conversation_id ~filename ~content_url ~unique_id
    ~file_type ()

let file_consent_send_reply ~alert ~config ~service_url ~conversation_id
    ~reply_to_id ~text () =
  send_reply ~alert ~config ~service_url ~conversation_id ~reply_to_id ~text ()

let handle_file_consent_invoke ~(config : Runtime_config.teams_config) json =
  Teams_file_consent.handle_file_consent_invoke ~fetch_token
    ~post_json_throttled ~send_reply:file_consent_send_reply ~config json

(** {1 Card action invoke handling} *)

(** Supported card action names for task controls. *)
type card_action =
  | TaskInspect of int
  | TaskContinue of int
  | TaskCancel of int
  | Unsupported

(** [parse_card_action json] extracts a card action from an invoke JSON payload.
    Returns [Unsupported] for unrecognized actions. *)
let parse_card_action (json : Yojson.Safe.t) : card_action =
  let open Yojson.Safe.Util in
  let name = try json |> member "name" |> to_string with _ -> "" in
  let value = try json |> member "value" |> to_string with _ -> "" in
  match name with
  | "task/inspect" -> (
      match int_of_string_opt value with
      | Some id -> TaskInspect id
      | None -> Unsupported)
  | "task/continue" -> (
      match int_of_string_opt value with
      | Some id -> TaskContinue id
      | None -> Unsupported)
  | "task/cancel" -> (
      match int_of_string_opt value with
      | Some id -> TaskCancel id
      | None -> Unsupported)
  | _ -> Unsupported

(** [tool_name_for_card_action action] returns the room policy tool name
    required for a given card action. *)
let tool_name_for_card_action = function
  | TaskInspect _ -> "background_task_list"
  | TaskContinue _ -> "background_task_resume"
  | TaskCancel _ -> "background_task_cancel"
  | Unsupported -> ""

(** [check_card_action_room_policy ~config ~session_key action] checks if the
    card action is allowed by room policy. Returns [Ok ()] if allowed,
    [Error msg] if denied. *)
let check_card_action_room_policy ~(config : Runtime_config.t) ?session_key
    (action : card_action) =
  let tool_name = tool_name_for_card_action action in
  if tool_name = "" then Ok ()
  else
    match session_key with
    | Some key ->
        if
          Option.is_some
            (Runtime_config.room_profile_tool_denial_for_session config
               ~session_key:key ~tool_name)
        then Error (Printf.sprintf "Action denied by room policy: %s" tool_name)
        else Ok ()
    | None -> Ok ()

(** [format_card_action_feedback action result] formats feedback for a card
    action result. Returns a human-readable message. *)
let format_card_action_feedback (action : card_action)
    (result : (string, string) result) =
  match (action, result) with
  | TaskInspect _, Ok msg -> Printf.sprintf "Inspect: %s" msg
  | TaskContinue _, Ok msg -> Printf.sprintf "Continue: %s" msg
  | TaskCancel _, Ok msg -> Printf.sprintf "Cancel: %s" msg
  | _, Error msg -> Printf.sprintf "Denied: %s" msg
  | Unsupported, _ -> "Unsupported action"

let handle_invoke ~(config : Runtime_config.teams_config)
    ~(session_manager : Session.t) ~auth_header body_str =
  let open Lwt.Syntax in
  let* auth_result = verify_auth ~config auth_header in
  match auth_result with
  | Error reason ->
      Logs.warn (fun m -> m "Teams: invoke auth failed: %s" reason);
      let response = Teams_file_consent.unauthorized_invoke_response () in
      Lwt.return
        (response.status_code, Teams_file_consent.invoke_response_body response)
  | Ok () -> (
      try
        let json = Yojson.Safe.from_string body_str in
        let name =
          try Yojson.Safe.Util.(json |> member "name" |> to_string)
          with _ -> ""
        in
        (* Route to appropriate handler *)
        match name with
        | "fileConsent/invoke" ->
            let* response = handle_file_consent_invoke ~config json in
            Lwt.return
              ( response.status_code,
                Teams_file_consent.invoke_response_body response )
        | "task/inspect" | "task/continue" | "task/cancel" -> (
            (* Handle task control card actions.
               Resolve room context from the conversation to preserve
               requester identity, room profile, admin/guest policy, and
               effective access snapshot through the card action. *)
            let action = parse_card_action json in
            let conversation_id =
              try
                Yojson.Safe.Util.(json |> member "conversation" |> member "id")
                |> Yojson.Safe.Util.to_string
              with _ -> ""
            in
            let user_id =
              try
                Yojson.Safe.Util.(json |> member "from" |> member "id")
                |> Yojson.Safe.Util.to_string
              with _ -> ""
            in
            let session_key =
              if conversation_id <> "" then
                Some
                  (resolve_session_key ~session_manager ~team_id:""
                     ~conversation_id ())
              else None
            in
            let is_admin =
              match Session.get_db session_manager with
              | Some db when user_id <> "" ->
                  Admin.is_admin ~db ~channel:"teams" ~sender_id:user_id
              | _ -> false
            in
            let user_group = if is_admin then "admin" else "guest" in
            (match session_key with
            | Some key when conversation_id <> "" ->
                Logs.info (fun m ->
                    m
                      "Teams: card action %s preserving context conv=%s \
                       session=%s user_group=%s"
                      name conversation_id key user_group)
            | _ -> ());
            let policy_result =
              check_card_action_room_policy
                ~config:(Session.get_config session_manager)
                ?session_key action
            in
            match policy_result with
            | Error msg ->
                (* Return 403 Forbidden for denied actions *)
                let response =
                  Teams_file_consent.make_invoke_response
                    ~body:(`Assoc [ ("message", `String msg) ])
                    403
                in
                Lwt.return
                  ( response.status_code,
                    Teams_file_consent.invoke_response_body response )
            | Ok () ->
                (* Action allowed — process it *)
                let feedback =
                  match action with
                  | TaskInspect id ->
                      (* Return task details *)
                      let task_info =
                        match Session.get_db session_manager with
                        | Some db -> (
                            match Background_task.get_task ~db ~id with
                            | Some task ->
                                let status_str =
                                  Background_task.string_of_status task.status
                                in
                                Printf.sprintf
                                  "Task #%d: %s\nStatus: %s\nRunner: %s" id
                                  (Option.value task.description
                                     ~default:"No description")
                                  status_str
                                  (Background_task.string_of_runner task.runner)
                            | None -> Printf.sprintf "Task #%d not found" id)
                        | None -> "Database not available"
                      in
                      Ok task_info
                  | TaskContinue id ->
                      (* Resume task *)
                      let result =
                        match Session.get_db session_manager with
                        | Some db -> (
                            match
                              Background_task.request_resume ~message:None ~db
                                ~id
                            with
                            | Ok msg -> Ok msg
                            | Error msg -> Error msg)
                        | None -> Error "Database not available"
                      in
                      result
                  | TaskCancel id ->
                      (* Cancel task *)
                      let result =
                        match Session.get_db session_manager with
                        | Some db -> (
                            match
                              Background_task.cancel_with_signal
                                ~send_signal:Unix.kill ~db ~id ()
                            with
                            | Ok msg -> Ok msg
                            | Error msg -> Error msg)
                        | None -> Error "Database not available"
                      in
                      result
                  | Unsupported -> Error "Unsupported action"
                in
                let status_code =
                  match feedback with Ok _ -> 200 | Error _ -> 400
                in
                let response =
                  Teams_file_consent.make_invoke_response
                    ~body:
                      (`Assoc
                         [
                           ( "message",
                             `String
                               (format_card_action_feedback action feedback) );
                         ])
                    status_code
                in
                Lwt.return
                  ( response.status_code,
                    Teams_file_consent.invoke_response_body response ))
        | _ ->
            Logs.debug (fun m -> m "Teams: unhandled invoke name=%s" name);
            let response = Teams_file_consent.ok_invoke_response () in
            Lwt.return
              ( response.status_code,
                Teams_file_consent.invoke_response_body response )
      with exn ->
        Logs.err (fun m ->
            m "Teams: invoke handler error: %s" (Printexc.to_string exn));
        let response = Teams_file_consent.ok_invoke_response () in
        Lwt.return
          ( response.status_code,
            Teams_file_consent.invoke_response_body response ))

let make_status_notifier ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~reply_to_id : Status_message.notifier =
  {
    send =
      (fun ?parse_mode:_ text ->
        send_reply ~alert:false ~config ~service_url ~conversation_id
          ~reply_to_id ~text ());
    edit =
      (fun msg_id ?parse_mode:_ text ->
        let open Lwt.Syntax in
        let* () =
          edit_activity ~config ~service_url ~conversation_id
            ~activity_id:msg_id ~text ()
        in
        Lwt.return None);
    delete =
      (fun msg_id ->
        delete_activity ~config ~service_url ~conversation_id
          ~activity_id:msg_id ());
  }

let send_message ~(config : Runtime_config.teams_config) ~channel_id ~text =
  let service_url, conversation_id = decode_channel_id channel_id in
  let effective_service_url =
    if service_url = "" then config.service_url else service_url
  in
  send_reply ~config ~service_url:effective_service_url ~conversation_id
    ~reply_to_id:"" ~text ()

let is_team_allowed ~(config : Runtime_config.teams_config) ~team_id =
  match config.allow_teams with [ "*" ] -> true | ids -> List.mem team_id ids

let is_user_allowed ~(config : Runtime_config.teams_config) ~user_id =
  match config.allow_users with [ "*" ] -> true | ids -> List.mem user_id ids

(* Strip <at>...</at> mention tags from Teams message text *)
let strip_at_mentions text =
  (* Simple state machine to remove <at>...</at> tags *)
  let len = String.length text in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if
      !i + 3 < len
      && text.[!i] = '<'
      && text.[!i + 1] = 'a'
      && text.[!i + 2] = 't'
      && text.[!i + 3] = '>'
    then begin
      (* skip to </at> *)
      let j = ref (!i + 4) in
      while
        !j + 4 < len
        && not
             (text.[!j] = '<'
             && text.[!j + 1] = '/'
             && text.[!j + 2] = 'a'
             && text.[!j + 3] = 't'
             && text.[!j + 4] = '>')
      do
        incr j
      done;
      if !j + 4 < len then i := !j + 5
      else if !j + 4 = len then begin
        (* Closing </at> not found — emit remaining text *)
        Buffer.add_string buf (String.sub text !i (len - !i));
        i := len
      end
      else i := len
    end
    else begin
      Buffer.add_char buf text.[!i];
      incr i
    end
  done;
  String.trim (Buffer.contents buf)

(* Main webhook handler — called from http_server.ml with raw body.
   Responds asynchronously; caller should return 202 immediately. *)
let handle_webhook ~(config : Runtime_config.teams_config)
    ~(session_manager : Session.t) ?(send_reply_fn = send_reply)
    ?(send_adaptive_card_fn = send_adaptive_card) ?event_limiter ?turn_fn
    ~auth_header body_str =
  let open Lwt.Syntax in
  let send_reply = send_reply_fn in
  let send_adaptive_card = send_adaptive_card_fn in
  let session_turn = match turn_fn with Some f -> f | None -> Session.turn in
  (* Verify JWT claims *)
  let* auth_result = verify_auth ~config auth_header in
  match auth_result with
  | Error reason ->
      Logs.warn (fun m -> m "Teams: auth failed: %s" reason);
      Lwt.return_unit
  | Ok () -> (
      match Teams_activity_parser.parse_activity body_str with
      | None -> Lwt.return_unit
      | Some
          {
            activity_id;
            service_url;
            conversation_id;
            reply_to_id;
            user_id;
            user_name;
            team_id;
            text = raw_text;
            is_group;
            is_external;
            tenant_id;
            mentioned_ids;
            attachments = parsed_attachments;
          } -> (
          let db = Session.get_db session_manager in
          if dedup_seen_persistent ~db ~conversation_id ~activity_id then
            Lwt.return_unit
          else
            let text = strip_at_mentions raw_text in
            if text = "" && parsed_attachments = [] then Lwt.return_unit
            else
              (* In group chats, only process if bot was mentioned or
                 addressed *)
              let bot_mentioned =
                if is_group then
                  let bot_id_prefix = "28:" ^ config.app_id in
                  List.exists
                    (fun mid -> mid = config.app_id || mid = bot_id_prefix)
                    mentioned_ids
                else false
              in
              if
                not
                  (Group_chat_filter.should_respond ~is_group ~bot_mentioned
                     ~is_reply_to_bot:false ~bot_name:"clawq" text)
              then begin
                Logs.debug (fun m ->
                    m
                      "Teams: ignoring unaddressed group message conv=%s \
                       user=%s"
                      conversation_id user_id);
                let eff_tid = if team_id = "" then "personal" else team_id in
                (if room_has_profile_binding ~session_manager ~conversation_id
                 then
                   record_scoped_room_history_if_bound ~session_manager
                     ~team_id:eff_tid ~conversation_id ~user_id ~user_name ~text
                 else
                   let cfg = Session.get_config session_manager in
                   if
                     Connector_capabilities.should_capture_history
                       ~enabled:cfg.connector_history.enabled
                       Connector_capabilities.teams
                   then begin
                     let hist_key =
                       resolve_session_key ~session_manager ~team_id:eff_tid
                         ~conversation_id ~reply_to_id ()
                     in
                     let db =
                       if cfg.connector_history.persist_to_db then
                         Session.get_db session_manager
                       else None
                     in
                     Connector_history.record ?db
                       ~persist:cfg.connector_history.persist_to_db
                       ~key:hist_key ~channel_type:"teams"
                       ~max:cfg.connector_history.max_messages
                       ~sender_name:user_name ~sender_id:user_id ~text ()
                   end);
                Lwt.return_unit
              end
              else
                let effective_team_id =
                  if team_id = "" then "personal" else team_id
                in
                Logs.info (fun m ->
                    m "Teams: message from user=%s (id=%s) team=%s conv=%s"
                      (if user_name <> "" then user_name else user_id)
                      user_id effective_team_id conversation_id);
                if is_external || tenant_id <> None then
                  Logs.info (fun m ->
                      m
                        "Teams: external room detected conv=%s is_external=%b \
                         tenant_id=%s"
                        conversation_id is_external
                        (Option.value tenant_id ~default:""));
                if not (is_team_allowed ~config ~team_id:effective_team_id) then (
                  Logs.warn (fun m ->
                      m "Teams: ignoring message from unauthorized team=%s"
                        effective_team_id);
                  Lwt.return_unit)
                else if not (is_user_allowed ~config ~user_id) then (
                  Logs.warn (fun m ->
                      m
                        "Teams: ignoring message from unauthorized user=%s \
                         (id=%s)"
                        (if user_name <> "" then user_name else user_id)
                        user_id);
                  Lwt.return_unit)
                else
                  let effective_service_url =
                    if service_url = "" then config.service_url else service_url
                  in
                  let key =
                    resolve_session_key ~session_manager
                      ~team_id:effective_team_id ~conversation_id ~reply_to_id
                      ()
                  in
                  let sender_name =
                    if user_name = "" then None else Some user_name
                  in
                  (* @mention the sender in group chats so they get a
                   notification. Only on final responses and ask_user_question
                   prompts — not on intermediate streaming updates (notify). *)
                  let mention =
                    if
                      is_group && user_name <> ""
                      && config.mention_mode <> "none"
                    then Some { mention_id = user_id; mention_name = user_name }
                    else None
                  in
                  let send_text text =
                    let open Lwt.Syntax in
                    let* _id =
                      send_reply ~alert:true ~config
                        ~service_url:effective_service_url ~conversation_id
                        ~reply_to_id:activity_id ~text ?mention ()
                    in
                    Lwt.return_unit
                  in
                  let limiter_key = conversation_id ^ ":" ^ user_id in
                  let* rate_decision =
                    check_incoming_rate_limit ?event_limiter ~limiter_key ()
                  in
                  match rate_decision with
                  | Rate_limited { should_warn } ->
                      if should_warn then
                        send_text incoming_rate_limited_message
                      else Lwt.return_unit
                  | Allowed -> (
                      (* Ensure a typing indicator watcher is running for this
                       session. The watcher tracks Session live_activity and
                       sends typing activities while the session is active. *)
                      let typing_watcher =
                        Typing_indicator.ensure_session_typing_watcher
                          ~session_mgr:session_manager ~key
                          ~send_action:(fun () ->
                            send_typing_activity ~config
                              ~service_url:effective_service_url
                              ~conversation_id)
                          ~interval:3.0 ~idle_timeout:300.0
                      in
                      let refresh_typing () = typing_watcher.refresh () in
                      let skill_names =
                        List.map
                          (fun (s : Skills.skill_md_meta) -> s.md_name)
                          (Skills.available_skills ())
                      in
                      let slash_text = normalize_clawq_slash_text text in
                      let* ( cmd_result,
                             text,
                             skill_injections,
                             _loaded_skill_name ) =
                        match Slash_commands.handle ~skill_names slash_text with
                        | Slash_commands.SkillInvoke (name, args) -> (
                            if
                              args = ""
                              && Session.skill_loaded_in_context session_manager
                                   ~key name
                            then
                              Lwt.return
                                (Slash_commands.NotACommand, text, [], None)
                            else
                              let* result =
                                Skills.expand_slash_skill ~name ~args ()
                              in
                              match result with
                              | Ok r ->
                                  Lwt.return
                                    ( Slash_commands.NotACommand,
                                      text,
                                      [ r.skill_injection ],
                                      Some name )
                              | Error err_msg ->
                                  Lwt.return
                                    ( Slash_commands.Reply err_msg,
                                      text,
                                      [],
                                      None ))
                        | Slash_commands.InjectConnectorHistory count -> (
                            let cfg = Session.get_config session_manager in
                            let hist_key =
                              resolve_session_key ~session_manager
                                ~team_id:effective_team_id ~conversation_id
                                ~reply_to_id ()
                            in
                            let db =
                              if cfg.connector_history.persist_to_db then
                                Session.get_db session_manager
                              else None
                            in
                            match
                              Connector_history.get_formatted_for_key ?db
                                ~key:hist_key ~count ()
                            with
                            | Some (context, n) ->
                                let* _id =
                                  send_reply ~alert:false ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:
                                      (Printf.sprintf
                                         "Last %d chat msgs loaded into context"
                                         n)
                                    ()
                                in
                                Lwt.return
                                  ( Slash_commands.NotACommand,
                                    Printf.sprintf
                                      "[Loaded %d messages from channel \
                                       history]"
                                      n,
                                    [ context ],
                                    None )
                            | None ->
                                Lwt.return
                                  ( Slash_commands.Reply
                                      "No connector history available. Ensure \
                                       connector_history.enabled is true in \
                                       config. Buffer captures unaddressed \
                                       group messages received since daemon \
                                       started (or from DB if persist_to_db is \
                                       on).",
                                    text,
                                    [],
                                    None ))
                        | other -> Lwt.return (other, text, [], None)
                      in
                      let is_admin =
                        match Session.get_db session_manager with
                        | Some db ->
                            Admin.is_admin ~db ~channel:"teams"
                              ~sender_id:user_id
                        | None -> false
                      in
                      let user_group = if is_admin then "admin" else "guest" in
                      let cmd_result =
                        Slash_commands.gate_admin ~is_admin cmd_result
                      in
                      (match cmd_result with
                      | InjectConnectorHistory _ -> ()
                      | _ ->
                          record_scoped_room_history_if_bound ~session_manager
                            ~team_id:effective_team_id ~conversation_id ~user_id
                            ~user_name ~text);
                      match cmd_result with
                      | RegisterAsAdminOtc None ->
                          let _code =
                            Admin.generate_otc ~channel:"teams"
                              ~sender_id:user_id
                          in
                          send_text
                            "Admin registration initiated. Check the daemon \
                             console/logs for your one-time code, then run: \
                             /register_as_admin_otc CODE"
                      | RegisterAsAdminOtc (Some code) -> (
                          match Session.get_db session_manager with
                          | Some db -> (
                              match
                                Admin.verify_otc ~db ~channel:"teams"
                                  ~sender_id:user_id ~code
                              with
                              | Ok () ->
                                  send_text "Successfully registered as admin."
                              | Error err_msg -> send_text err_msg)
                          | None -> send_text "Database not available.")
                      | AdminRequired _ -> assert false
                      | InjectConnectorHistory _ ->
                          Lwt.return_unit (* unreachable: preprocessed above *)
                      | SkillInvoke _ ->
                          Lwt.return_unit (* unreachable: preprocessed above *)
                      | Followup action ->
                          let followup_channel_id =
                            encode_channel_id ~service_url:effective_service_url
                              ~conversation_id
                          in
                          Connector_dispatch.dispatch_followup
                            ~session_mgr:session_manager ~key
                            ~connector_name:"teams"
                            ~channel_id:followup_channel_id
                            ~channel_name:"teams"
                            ~channel_type:(if is_group then "group" else "dm")
                            ?sender_name ~message_id:activity_id ~user_id
                            ~is_admin ~send_reply:send_text action
                      | NotACommand -> (
                          (* Register status message factory and capabilities *)
                          if
                            Option.is_none
                              (Session.find_connector_capabilities
                                 session_manager ~key)
                          then
                            Session.register_connector_capabilities
                              session_manager ~key Connector_capabilities.teams;
                          Session.register_status_message_factory
                            session_manager ~key (fun () ->
                              let notifier =
                                make_status_notifier ~config
                                  ~service_url:effective_service_url
                                  ~conversation_id ~reply_to_id:activity_id
                              in
                              Status_message.create ~notifier
                                ~parse_mode:"Teams" ());
                          (* Register alerting notifier for ask_user_question *)
                          Session.register_alert_channel_notifier
                            session_manager ~key (fun reply_text ->
                              let* _id =
                                send_reply ~alert:true ~config
                                  ~service_url:effective_service_url
                                  ~conversation_id ~reply_to_id:activity_id
                                  ~text:reply_text ?mention ()
                              in
                              refresh_typing ();
                              Lwt.return_unit);
                          (* Register rich notifier for Adaptive Cards *)
                          if
                            Option.is_none
                              (Session.find_rich_notifier session_manager ~key)
                          then
                            Session.register_rich_notifier session_manager ~key
                              (fun msg ->
                                match msg with
                                | Rich_message.TextWithButtons
                                    { text; button_rows } ->
                                    let card =
                                      Question_presenter
                                      .build_teams_card_from_buttons ~text
                                        ~button_rows
                                    in
                                    let* _id =
                                      send_adaptive_card ~config
                                        ~service_url:effective_service_url
                                        ~conversation_id
                                        ~reply_to_id:activity_id ~card ()
                                    in
                                    Lwt.return
                                      Rich_message.
                                        { message_id = "0"; callback_ids = [] }
                                | Rich_message.Poll
                                    { question; options; allows_multiple } ->
                                    let card =
                                      Question_presenter.build_teams_poll_card
                                        ~question ~options
                                    in
                                    ignore allows_multiple;
                                    let* _id =
                                      send_adaptive_card ~config
                                        ~service_url:effective_service_url
                                        ~conversation_id
                                        ~reply_to_id:activity_id ~card ()
                                    in
                                    Lwt.return
                                      Rich_message.
                                        { message_id = "0"; callback_ids = [] }
                                | Rich_message.Text text ->
                                    let* _id =
                                      send_reply ~alert:false ~config
                                        ~service_url:effective_service_url
                                        ~conversation_id
                                        ~reply_to_id:activity_id ~text ()
                                    in
                                    Lwt.return
                                      Rich_message.
                                        { message_id = "0"; callback_ids = [] }
                                | Rich_message.FileAttachment _ ->
                                    Lwt.return
                                      Rich_message.
                                        { message_id = "0"; callback_ids = [] });
                          let* result =
                            Session.with_registered_notifier session_manager
                              ~key
                              ~notify:(fun reply_text ->
                                (* No mention on intermediate updates — mention only
                             on the final response to avoid repeated tagging. *)
                                let* _id =
                                  send_reply ~alert:false ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:reply_text ()
                                in
                                refresh_typing ();
                                Lwt.return_unit)
                              (fun () ->
                                Lwt.catch
                                  (fun () ->
                                    let full_config =
                                      Session.get_config session_manager
                                    in
                                    (* Partition audio attachments for transcription *)
                                    let audio_atts, non_audio_atts =
                                      List.partition
                                        (fun (a :
                                               Teams_activity_parser
                                               .teams_attachment) ->
                                          Voice_transcription.is_audio_mime
                                            a.content_type)
                                        parsed_attachments
                                    in
                                    (* Single token fetch for audio + attachment downloads *)
                                    let needs_token =
                                      full_config.security
                                        .attachment_downloads_enabled
                                      && (audio_atts <> []
                                        || non_audio_atts <> [])
                                    in
                                    let* attachment_token =
                                      if needs_token then fetch_token ~config
                                      else Lwt.return None
                                    in
                                    let* transcription_prefix =
                                      if
                                        audio_atts <> []
                                        && full_config.security
                                             .attachment_downloads_enabled
                                      then
                                        let audio_headers =
                                          match attachment_token with
                                          | Some tok ->
                                              [
                                                ( "Authorization",
                                                  "Bearer " ^ tok );
                                              ]
                                          | None -> []
                                        in
                                        let* texts =
                                          Lwt_list.map_s
                                            (fun (a :
                                                   Teams_activity_parser
                                                   .teams_attachment) ->
                                              match
                                                Voice_transcription.validate
                                                  ~config:full_config
                                                  ~filename:a.name
                                                  ~mime_type:
                                                    (Some a.content_type)
                                                  ~size:None
                                                  ~duration_seconds:None
                                              with
                                              | Error reason ->
                                                  Logs.info (fun m ->
                                                      m
                                                        "Teams voice skipped \
                                                         %s: %s"
                                                        a.name
                                                        (Voice_transcription
                                                         .skip_reason_to_string
                                                           reason));
                                                  Lwt.return ""
                                              | Ok () ->
                                                  Lwt.catch
                                                    (fun () ->
                                                      let* _status, audio_data =
                                                        Http_client.get
                                                          ~uri:a.content_url
                                                          ~headers:audio_headers
                                                      in
                                                      let notifier =
                                                        make_status_notifier
                                                          ~config
                                                          ~service_url:
                                                            effective_service_url
                                                          ~conversation_id
                                                          ~reply_to_id:
                                                            activity_id
                                                      in
                                                      Voice_transcription
                                                      .transcribe_with_progress
                                                        ~config:full_config
                                                        ~notifier ~audio_data
                                                        ~filename:a.name ())
                                                    (fun exn ->
                                                      Logs.err (fun m ->
                                                          m
                                                            "Teams voice \
                                                             transcription \
                                                             failed %s: %s"
                                                            a.name
                                                            (Printexc.to_string
                                                               exn));
                                                      Lwt.return ""))
                                            audio_atts
                                        in
                                        Lwt.return
                                          (String.concat ""
                                             (List.filter
                                                (fun s -> s <> "")
                                                texts))
                                      else Lwt.return ""
                                    in
                                    let effective_text =
                                      if transcription_prefix <> "" then
                                        transcription_prefix ^ "\n" ^ text
                                      else text
                                    in
                                    let* content_parts, att_list, message =
                                      if
                                        non_audio_atts <> []
                                        && full_config.security
                                             .attachment_downloads_enabled
                                      then
                                        let headers =
                                          match attachment_token with
                                          | Some tok ->
                                              [
                                                ( "Authorization",
                                                  "Bearer " ^ tok );
                                              ]
                                          | None -> []
                                        in
                                        let workspace =
                                          Runtime_config.effective_workspace
                                            full_config
                                        in
                                        let metas =
                                          List.map
                                            (fun (a :
                                                   Teams_activity_parser
                                                   .teams_attachment) ->
                                              Attachment_download.
                                                {
                                                  url = a.content_url;
                                                  filename = a.name;
                                                  mime_type =
                                                    Some a.content_type;
                                                  size = None;
                                                })
                                            non_audio_atts
                                        in
                                        Attachment_download.process_attachments
                                          metas ~headers ~workspace
                                          ~db:(Session.get_db session_manager)
                                          ~session_key:key ~source:"teams"
                                          ~content_parts:[] ~attachments:[]
                                          ~message:effective_text
                                      else
                                        let placeholder =
                                          if non_audio_atts <> [] then
                                            let names =
                                              List.map
                                                (fun (a :
                                                       Teams_activity_parser
                                                       .teams_attachment) ->
                                                  Printf.sprintf
                                                    "\n\
                                                     [Attachment: %s (download \
                                                     disabled)]"
                                                    a.name)
                                                non_audio_atts
                                            in
                                            effective_text
                                            ^ String.concat "" names
                                          else effective_text
                                        in
                                        Lwt.return ([], [], placeholder)
                                    in
                                    (* Auto-inject bounded room context for
                                       profile-bound rooms with connector
                                       history enabled. *)
                                    let skill_injections =
                                      let ctx_key =
                                        resolve_session_key ~session_manager
                                          ~team_id:effective_team_id
                                          ~conversation_id ()
                                      in
                                      match
                                        Teams_context_capture
                                        .capture_room_context ~session_manager
                                          ~has_binding:
                                            (room_has_profile_binding
                                               ~session_manager)
                                          ~session_key:ctx_key ~conversation_id
                                      with
                                      | Some ctx -> ctx :: skill_injections
                                      | None -> skill_injections
                                    in
                                    let* response =
                                      session_turn session_manager ~key ~message
                                        ~content_parts ~attachments:att_list
                                        ~skill_injections ~channel_name:"teams"
                                        ~channel_type:
                                          (if is_group then "group" else "dm")
                                        ~user_group ~channel:"teams"
                                        ~channel_id:
                                          (encode_channel_id
                                             ~service_url:effective_service_url
                                             ~conversation_id)
                                        ~sender_id:user_id ?sender_name
                                        ~has_external_users:is_external ()
                                    in
                                    Lwt.return (Ok response))
                                  (fun exn ->
                                    Lwt.return
                                      (Error (user_facing_error_of_exn exn))))
                          in
                          match result with
                          | Ok response ->
                              if Session.should_suppress_response response then
                                Lwt.return_unit
                              else
                                let* _id =
                                  send_reply ~alert:true ~config
                                    ~service_url:effective_service_url
                                    ~conversation_id ~reply_to_id:activity_id
                                    ~text:response ?mention ()
                                in
                                Lwt.return_unit
                          | Error err ->
                              Logs.err (fun m ->
                                  m
                                    "Teams: agent error for conv=%s user=%s \
                                     (id=%s): %s"
                                    conversation_id
                                    (if user_name <> "" then user_name
                                     else user_id)
                                    user_id err);
                              send_text (agent_error_message err))
                      | Reply text -> send_text text
                      | FormattedReply fn ->
                          let text = fn Format_adapter.Teams in
                          send_text text
                      | Help ->
                          let show_test = is_admin in
                          let text =
                            Slash_commands.format_help
                              ~connector:Format_adapter.Teams ~show_test
                              ~is_admin ()
                          in
                          send_text text
                      | Menu page ->
                          let full_config =
                            Session.get_config session_manager
                          in
                          let card_json =
                            Slash_commands_manifest.menu_adaptive_card_json
                              ~page ~is_admin ~config:full_config
                              ~session_key:key ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | Reset ->
                          let* active_bg_tasks =
                            Session.reset session_manager ~key
                          in
                          send_text
                            (Slash_commands_fmt.format_reset
                               ~connector:Format_adapter.Teams ~active_bg_tasks)
                      | Compact -> (
                          let notifier =
                            make_status_notifier ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                          in
                          let* compact_result =
                            Session.compact session_manager ~key ~notifier ()
                          in
                          match compact_result with
                          | Ok _ -> Lwt.return_unit
                          | Error err ->
                              send_text
                                (Printf.sprintf "Compaction failed: %s" err))
                      | RuntimeCtx ->
                          let* text =
                            Session.runtime_context_block session_manager ~key
                          in
                          send_text text
                      | Context ->
                          send_text
                            (Slash_commands_context.format
                               ~connector:Format_adapter.Teams
                               ~session_mgr:session_manager ~session_key:key)
                      | Uptime ->
                          let raw =
                            Daemon_status.daemon_uptime_reply
                              ~pid:(Daemon_status.read_current_daemon_pid ())
                          in
                          send_text
                            (Slash_commands_fmt.format_uptime
                               ~connector:Format_adapter.Teams raw)
                      | Status ->
                          let text =
                            Slash_commands.format_status
                              ~connector:Format_adapter.Teams
                              ~db:(Session.get_db session_manager)
                              ~session_count:
                                (Session.session_count session_manager)
                              ~active_count:
                                (Session.active_session_count session_manager)
                              ()
                          in
                          send_text text
                      | Thinking Slash_commands.ShowThinking ->
                          let current =
                            (Session.get_config session_manager).agent_defaults
                              .reasoning_effort
                          in
                          send_text
                            (Slash_commands_fmt.format_thinking_status
                               ~connector:Format_adapter.Teams current)
                      | Thinking (Slash_commands.SetThinking level) ->
                          let connector = Format_adapter.Teams in
                          let cfg = Session.get_config session_manager in
                          let previous = cfg.agent_defaults.reasoning_effort in
                          let text =
                            match Config_set.set_reasoning_effort level with
                            | Ok () ->
                                Session.update_config ~source:"teams"
                                  session_manager
                                  {
                                    cfg with
                                    agent_defaults =
                                      {
                                        cfg.agent_defaults with
                                        reasoning_effort = level;
                                      };
                                  };
                                Slash_commands_fmt.format_thinking_set
                                  ~connector ~previous level
                            | Error err ->
                                "Failed to set thinking level: " ^ err
                          in
                          send_text text
                      | ShowThinking action ->
                          let connector = Format_adapter.Teams in
                          let cfg = Session.get_config session_manager in
                          let current = cfg.agent_defaults.show_thinking in
                          let text =
                            match action with
                            | Slash_commands.ShowThinkingStatus ->
                                Slash_commands_fmt.format_show_thinking_status
                                  ~connector current
                            | Slash_commands.ToggleShowThinking -> (
                                let new_val = not current in
                                match Config_set.set_show_thinking new_val with
                                | Ok () ->
                                    Session.update_config ~source:"teams"
                                      session_manager
                                      {
                                        cfg with
                                        agent_defaults =
                                          {
                                            cfg.agent_defaults with
                                            show_thinking = new_val;
                                          };
                                      };
                                    Slash_commands_fmt
                                    .format_show_thinking_toggle ~connector
                                      new_val
                                | Error err ->
                                    "Failed to update show_thinking: " ^ err)
                          in
                          send_text text
                      | Heartbeat action ->
                          let connector = Format_adapter.Teams in
                          let text =
                            match action with
                            | Slash_commands.HeartbeatStatus ->
                                Slash_commands_fmt.format_heartbeat_status
                                  ~connector
                                  (Session.session_heartbeat_status_text
                                     session_manager ~key)
                            | Slash_commands.SetHeartbeat enabled -> (
                                match
                                  Session.set_session_heartbeat session_manager
                                    ~key ~enabled
                                with
                                | Ok () ->
                                    Slash_commands_fmt.format_heartbeat_set
                                      ~connector enabled key
                                | Error err -> err)
                          in
                          send_text text
                      | Debug action ->
                          let connector = Format_adapter.Teams in
                          let text =
                            match action with
                            | Slash_commands.DebugStatus ->
                                Slash_commands_fmt.format_debug_status
                                  ~connector
                                  (Session.session_debug_status_text
                                     session_manager ~key)
                            | Slash_commands.SetDebug enabled -> (
                                match
                                  Session.set_session_debug session_manager ~key
                                    ~enabled
                                with
                                | Ok () ->
                                    Slash_commands_fmt.format_debug_set
                                      ~connector enabled key
                                | Error err -> err)
                          in
                          send_text text
                      | Delegate (agent_name, prompt) ->
                          let* () =
                            send_text "Delegating to a temporary session..."
                          in
                          Session.delegate_turn session_manager ~parent_key:key
                            ~debug_notify:send_text ?agent_name ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | AgentInvoke (agent_name, prompt) ->
                          let* () =
                            send_text
                              (Printf.sprintf "Invoking agent '%s'..."
                                 agent_name)
                          in
                          Session.agent_invoke_turn session_manager ~agent_name
                            ~parent_key:key ~debug_notify:send_text ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | AgentMenu page ->
                          let card_json =
                            Slash_commands_manifest
                            .agent_menu_adaptive_card_json ~page ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | ModelMenu page ->
                          let card_json =
                            Slash_commands_manifest
                            .model_menu_adaptive_card_json ~page ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | ThinkingMenu ->
                          let card_json =
                            Slash_commands_manifest
                            .thinking_menu_adaptive_card_json ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | ConfigMenu page ->
                          let card_json =
                            Slash_commands_manifest
                            .config_menu_adaptive_card_json ~page ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | SkillsMenu page ->
                          let show_test = is_admin in
                          let card_json =
                            Slash_commands_manifest
                            .skills_menu_adaptive_card_json ~show_test ~page ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | CostsMenu ->
                          let card_json =
                            Slash_commands_manifest
                            .costs_menu_adaptive_card_json ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | BgMenu ->
                          let cancellable =
                            match Session.get_db session_manager with
                            | Some db ->
                                let tasks, _ =
                                  Background_task.list_tasks_for_display ~db
                                in
                                List.filter_map
                                  (fun (t : Background_task.task) ->
                                    match t.status with
                                    | Running | Queued ->
                                        Some
                                          ( t.id,
                                            Background_task.string_of_runner
                                              t.runner )
                                    | _ -> None)
                                  tasks
                            | None -> []
                          in
                          let full_config =
                            Session.get_config session_manager
                          in
                          let card_json =
                            Slash_commands_manifest.bg_menu_adaptive_card_json
                              ~config:full_config ~session_key:key ~cancellable
                              ()
                          in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                              ~card:card_json ()
                          in
                          Lwt.return_unit
                      | ForkAnd (agent_name, prompt) ->
                          let* () = send_text "Forking session..." in
                          Session.fork_and_run session_manager ~parent_key:key
                            ~debug_notify:send_text ?agent_name ~prompt
                            ~send_reply:send_text ();
                          Lwt.return_unit
                      | Debate prompt -> (
                          match Session.get_db session_manager with
                          | Some db ->
                              let config = Session.get_config session_manager in
                              let on_llm_call_debug =
                                Session.debug_callback_for session_manager ~key
                                  (Some send_text)
                              in
                              let* text =
                                Debate.run_for_prompt ?on_llm_call_debug ~config
                                  ~db ~prompt ()
                              in
                              send_text text
                          | None -> send_text "Debate requires a database.")
                      | BashRun cmd ->
                          let config = Session.get_config session_manager in
                          let* result =
                            Slash_commands_bash.run_bash_command ~config
                              ~session_key:key cmd
                          in
                          let full_text =
                            Slash_commands_bash.format_result cmd result
                          in
                          let max_len = 25000 in
                          let text =
                            if String.length full_text <= max_len then full_text
                            else
                              String.sub full_text 0 max_len
                              ^ "\n...[truncated]"
                          in
                          send_text text
                      | DebugDumpChat -> (
                          let content =
                            Session.dump_json session_manager ~key
                          in
                          let timestamp =
                            Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
                          in
                          let safe_key =
                            String.map
                              (fun c ->
                                match c with
                                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' ->
                                    c
                                | _ -> '_')
                              key
                          in
                          let filename =
                            Printf.sprintf "session_%s_%d.json" safe_key
                              timestamp
                          in
                          let send_temp_download () =
                            let token =
                              Temp_downloads.add ~content
                                ~content_type:"application/json" ~filename
                                ~ttl_s:3600.0
                            in
                            let msg =
                              match Temp_downloads.download_url token with
                              | Some url ->
                                  Printf.sprintf
                                    "Session dump available for download (%d \
                                     bytes, expires in 1 hour):\n\n\
                                     %s"
                                    (String.length content) url
                              | None ->
                                  let max_len = 25000 in
                                  if String.length content <= max_len then
                                    content
                                  else
                                    Printf.sprintf
                                      "Session dump (truncated — configure \
                                       tunnel.url for full file download):\n\
                                       %s\n\
                                       ...\n\n\
                                       Full dump: %d bytes"
                                      (String.sub content 0 max_len)
                                      (String.length content)
                            in
                            send_text msg
                          in
                          let delivery =
                            select_file_upload_delivery
                              ~file_consent_cards:config.file_consent_cards
                              ~team_id ~is_group
                          in
                          match delivery with
                          | File_consent_card -> (
                              let size_bytes = String.length content in
                              let room_context =
                                consent_room_context ~session_manager
                                  ~conversation_id ~user_group
                                  ?access_snapshot_id:None ()
                              in
                              let consent_id =
                                store_pending_consent ?room_context ~content
                                  ~filename ~content_type:"application/json"
                                  ~ttl_s:600.0 ()
                              in
                              let* result =
                                send_file_consent_card ?room_context ~config
                                  ~service_url:effective_service_url
                                  ~conversation_id ~reply_to_id:activity_id
                                  ~filename ~description:"Session debug dump"
                                  ~size_bytes ~consent_id ()
                              in
                              match result with
                              | Ok () ->
                                  (* Also send the download link as a fallback
                                 in case the file upload consent flow fails *)
                                  send_temp_download ()
                              | Error err ->
                                  Logs.warn (fun m ->
                                      m
                                        "Teams: file consent card failed (%s), \
                                         falling back to temp download"
                                        err);
                                  send_temp_download ())
                          | Temp_download_url -> send_temp_download ())
                      | Tools ->
                          let show_test = is_admin in
                          let text =
                            match Session.get_tool_registry session_manager with
                            | Some reg ->
                                let tools, _ =
                                  Tool_registry.partition_skills reg
                                in
                                let tools =
                                  Skills.filter_visible_tools ~show_test tools
                                in
                                let skills =
                                  Skills.filter_visible_tools ~show_test
                                    (Skills.available_skills_as_tools ())
                                in
                                Slash_commands.format_tools
                                  ~connector:Format_adapter.Teams tools skills
                                  (Agent_template.available_templates ())
                            | None -> "Tools are not enabled."
                          in
                          send_text text
                      | Tasks ->
                          let raw =
                            match Session.get_db session_manager with
                            | Some db ->
                                Task_tree.init_schema db;
                                Task_tree.render_emoji_tree ~db ~session_key:key
                                  ()
                            | None -> "Tasks are not available (no database)."
                          in
                          send_text
                            (Slash_commands_fmt.format_tasks
                               ~connector:Format_adapter.Teams raw)
                      | TasksFull ->
                          let raw =
                            match Session.get_db session_manager with
                            | Some db ->
                                Task_tree.init_schema db;
                                Task_tree.render_tree_with_legend ~db
                                  ~session_key:key
                            | None -> "Tasks are not available (no database)."
                          in
                          send_text
                            (Slash_commands_fmt.format_tasks
                               ~connector:Format_adapter.Teams raw)
                      | Costs action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_costs
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Costs are not available (no database)."
                          in
                          send_text text
                      | Session action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands_sessions.format_session
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Sessions not available (no database)."
                          in
                          send_text text
                      | Usage action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_usage
                                  ~connector:Format_adapter.Teams ~db action
                            | None -> "Usage is not available (no database)."
                          in
                          send_text text
                      | Active ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let cfg = Session.get_config session_manager in
                                Slash_commands.format_active
                                  ~connector:Format_adapter.Teams ~db
                                  ~config:cfg ()
                            | None ->
                                "Active usage is not available (no database)."
                          in
                          send_text text
                      | Bg action -> (
                          match Session.get_db session_manager with
                          | None ->
                              send_text
                                "Background tasks are not available (no \
                                 database)."
                          | Some db -> (
                              match action with
                              | BgCancel id ->
                                  let text =
                                    match
                                      Background_task.cancel_with_signal
                                        ~send_signal:Unix.kill
                                        ~terminate_group:(fun
                                            ?grace_seconds:_
                                            ?wait_seconds:_
                                            pid
                                          ->
                                          Lwt.async (fun () ->
                                              Process_group.terminate pid))
                                        ~db ~id ()
                                    with
                                    | Ok msg -> msg
                                    | Error msg -> msg
                                  in
                                  send_text text
                              | _ ->
                                  let* text =
                                    Slash_commands.format_bg
                                      ~connector:Format_adapter.Teams ~db action
                                  in
                                  send_text text))
                      | WorkflowRun action ->
                          let* text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let config =
                                  Session.get_config session_manager
                                in
                                Slash_commands.format_workflow
                                  ~connector:Format_adapter.Teams ~db ~config
                                  ~room_id:key ~requester_id:user_id action
                            | None ->
                                Lwt.return
                                  "Workflow runs are not available (no \
                                   database)."
                          in
                          send_text text
                      | Cron action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_cron
                                  ~connector:Format_adapter.Teams ~db
                                  ~session_key:key
                                  ~is_admin:(user_group = "admin") action
                            | None -> "Cron is not available (no database)."
                          in
                          send_text text
                      | Bl action ->
                          let text =
                            Slash_commands.format_bl
                              ~connector:Format_adapter.Teams action
                          in
                          send_text text
                      | HeldItems action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_held_items
                                  ~connector:Format_adapter.Teams ~db action
                            | None ->
                                "Held items are not available (no database)."
                          in
                          send_text text
                      | Memories action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                Slash_commands.format_memories
                                  ~connector:Format_adapter.Teams ~db action
                            | None ->
                                "Memories are not available (no database)."
                          in
                          send_text text
                      | RoomsMemory action ->
                          let text =
                            match Session.get_db session_manager with
                            | Some db ->
                                let cfg = Session.get_config session_manager in
                                Slash_commands.format_room_memories
                                  ~connector:Format_adapter.Teams ~db ~cfg
                                  ~channel_id:conversation_id ~is_admin action
                            | None -> "Room memory commands require a database."
                          in
                          send_text text
                      | ExplainAccess ->
                          let cfg = Session.get_config session_manager in
                          let access_key =
                            Connector_dispatch.room_access_key cfg key
                          in
                          let explanation =
                            Access_explanation.create ~config:cfg
                              ~session_key:access_key ()
                          in
                          let text = Access_explanation.to_text explanation in
                          send_text
                            (Format_adapter.code_block Format_adapter.Teams text)
                      | WhatCanDo ->
                          let snap =
                            Teams_what_can_do.snapshot ~session_manager
                              ~conversation_id ()
                          in
                          let card = Teams_what_can_do.build_card ~snap () in
                          let* _id =
                            send_adaptive_card ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id ~card ()
                          in
                          Lwt.return_unit
                      | Rig action -> (
                          match action with
                          | RigList ->
                              let text = Rig.list_text () in
                              send_text text
                          | RigInstall name | RigAdjust name | RigRemove name
                            -> (
                              let act =
                                match action with
                                | RigInstall _ -> `Install
                                | RigAdjust _ -> `Adjust
                                | _ -> `Remove
                              in
                              let act_str =
                                match act with
                                | `Install -> "install"
                                | `Adjust -> "adjust"
                                | `Remove -> "remove"
                              in
                              match Rig.prompt_for ~name ~action:act with
                              | Error msg -> send_text msg
                              | Ok prompt ->
                                  let* () =
                                    send_text
                                      (Printf.sprintf
                                         "Running rig %s for '%s'..." act_str
                                         name)
                                  in
                                  Session.delegate_turn session_manager ~prompt
                                    ~parent_key:key ~debug_notify:send_text
                                    ~send_reply:send_text ();
                                  (match act with
                                  | `Install -> (
                                      match Rig.find_rig name with
                                      | Some rig ->
                                          Rig.mark_installed ~name
                                            ~version:rig.version
                                      | None -> ())
                                  | `Remove -> Rig.mark_removed ~name
                                  | `Adjust -> ());
                                  Lwt.return_unit))
                      | Repo action -> (
                          match Session.get_db session_manager with
                          | Some db ->
                              Slash_commands_repo.handle_repo_action ~db
                                ~session_key:key ~connector:Format_adapter.Teams
                                ~send_reply:send_text
                                ~set_cwd:(fun cwd ->
                                  Session.set_effective_cwd session_manager ~key
                                    ~cwd)
                                action
                          | None ->
                              send_text
                                "Repository management is not available (no \
                                 database).")
                      | Model action -> (
                          let open Slash_commands in
                          match action with
                          | ModelShow ->
                              let current =
                                Session.get_session_effective_model
                                  session_manager ~key
                              in
                              let prefs = Model_preferences.load () in
                              let usage_ranked =
                                List.filter_map
                                  (fun (m, c) ->
                                    if List.mem m prefs.favorites then None
                                    else Some (m, c))
                                  prefs.usage_counts
                              in
                              let text =
                                format_model_show
                                  ~connector:Format_adapter.Teams ~current
                                  ~favorites:prefs.favorites ~usage_ranked
                              in
                              send_text text
                          | ModelSet _ | ModelSetForce _ | ModelSetDefault _ ->
                              let* text =
                                Slash_commands_model.handle_model_set_action
                                  ~config_source:"teams" ~session_manager ~key
                                  action
                              in
                              send_text text
                          | ModelFav name ->
                              let prefs =
                                Model_preferences.toggle_favorite name
                              in
                              let status =
                                if List.mem name prefs.favorites then "added to"
                                else "removed from"
                              in
                              send_text
                                (Printf.sprintf "%s %s favorites" name status)
                          | ModelUnfav name ->
                              let _ = Model_preferences.remove_favorite name in
                              send_text
                                (Printf.sprintf "Removed from favorites: %s"
                                   name)
                          | ModelList (provider, availability) ->
                              let db_extras =
                                match Session.get_db session_manager with
                                | None -> []
                                | Some db ->
                                    Model_discovery.get_db_only_model_infos ~db
                                      ~provider_filter:provider ~availability ()
                              in
                              let models =
                                Models_catalog.to_plain_list
                                  ~provider_filter:provider ~availability
                                  ~db_extras ()
                                |> String.split_on_char '\n'
                                |> List.filter (fun s -> s <> "")
                              in
                              let text =
                                format_model_list
                                  ~connector:Format_adapter.Teams ~models
                                  ~provider
                              in
                              send_text text
                          | ModelUsage ->
                              let cfg = Session.get_config session_manager in
                              Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                              let results =
                                Provider_quota.get_all_cached ()
                                |> List.map (fun (_name, pq) -> pq)
                              in
                              let text =
                                Slash_commands.format_model_usage
                                  ~connector:Format_adapter.Teams ~config:cfg
                                  results
                              in
                              send_text text))))

(* Channel.S start — webhook-only, no polling loop needed *)
let start ~(config : Runtime_config.t) ~(_session_manager : Session.t) =
  match config.channels.teams with
  | None ->
      Logs.info (fun m -> m "Teams: no config found, skipping");
      Lwt.return_unit
  | Some tc ->
      Logs.info (fun m ->
          m "Teams: webhook channel ready at %s (app_id: %s...)" tc.webhook_path
            (String.sub tc.app_id 0 (min 8 (String.length tc.app_id))));
      Lwt.return_unit
