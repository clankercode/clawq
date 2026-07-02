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
