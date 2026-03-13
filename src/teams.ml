(* MS Teams channel via Microsoft Bot Framework webhook *)
(* See src/TEAMS_API.md for protocol and auth details *)

let max_message_chars = 28672
let dedup = Channel_util.Lru_dedup.create 500

let session_key ~team_id ~conversation_id =
  Printf.sprintf "teams:%s:%s" team_id conversation_id

type teams_activity = {
  activity_id : string;
  service_url : string;
  conversation_id : string;
  user_id : string;
  user_name : string;
  team_id : string;
  text : string;
  is_group : bool;
}

type mention = { mention_id : string; mention_name : string }

let dedup_seen id =
  if id = "" then false else Channel_util.Lru_dedup.check_and_mark dedup id

(* Token cache for outbound OAuth bearer tokens *)
let token_cache : (string * float) option ref = ref None

(* Fetch an OAuth 2.0 client_credentials token from Azure AD *)
let fetch_token ~(config : Runtime_config.teams_config) =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  match !token_cache with
  | Some (tok, exp) when now < exp -> Lwt.return (Some tok)
  | _ ->
      let uri =
        Printf.sprintf "https://login.microsoftonline.com/%s/oauth2/v2.0/token"
          config.tenant_id
      in
      let body =
        Printf.sprintf
          "grant_type=client_credentials&client_id=%s&client_secret=%s&scope=https%%3A%%2F%%2Fapi.botframework.com%%2F.default"
          (Uri.pct_encode ~component:`Query_value config.app_id)
          (Uri.pct_encode ~component:`Query_value config.app_secret)
      in
      (* Token endpoint requires form-encoded body, not JSON *)
      let uri_obj = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list
          [ ("Content-Type", "application/x-www-form-urlencoded") ]
      in
      let body_obj = Cohttp_lwt.Body.of_string body in
      let* resp, resp_body =
        Cohttp_lwt_unix.Client.post ~headers ~body:body_obj uri_obj
      in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let* body_str = Cohttp_lwt.Body.to_string resp_body in
      if status >= 200 && status < 300 then (
        try
          let json = Yojson.Safe.from_string body_str in
          let open Yojson.Safe.Util in
          let token = json |> member "access_token" |> to_string in
          let expires_in =
            try json |> member "expires_in" |> to_int with _ -> 3600
          in
          let expiry = now +. float_of_int expires_in -. 60.0 in
          token_cache := Some (token, expiry);
          Lwt.return (Some token)
        with exn ->
          Logs.err (fun m ->
              m "Teams: failed to parse token response: %s"
                (Printexc.to_string exn));
          Lwt.return None)
      else begin
        Logs.warn (fun m ->
            m
              "Teams: token fetch failed (HTTP %d) for app_id=%s tenant_id=%s: \
               %s"
              status config.app_id config.tenant_id body_str);
        Lwt.return None
      end

let test_connection ~(config : Runtime_config.teams_config) =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Lwt.return
        (Error
           "OAuth token fetch failed — check app_id, app_secret, and tenant_id \
            in config, and verify the client secret has not expired in Azure")
  | Some _ ->
      Lwt.return
        (Ok
           (Printf.sprintf
              "Teams connection OK\n\
              \  app_id:       %s\n\
              \  tenant_id:    %s\n\
              \  webhook_path: %s\n\
              \  OAuth token:  fetched successfully"
              config.app_id config.tenant_id config.webhook_path))

(* Decode a base64url-encoded string (no padding required) *)
let base64url_decode s =
  (* Convert base64url to standard base64 *)
  let n = String.length s in
  let buf = Buffer.create (n + 4) in
  String.iter
    (fun c ->
      match c with
      | '-' -> Buffer.add_char buf '+'
      | '_' -> Buffer.add_char buf '/'
      | c -> Buffer.add_char buf c)
    s;
  (* Add padding *)
  let pad = (4 - (Buffer.length buf mod 4)) mod 4 in
  for _ = 1 to pad do
    Buffer.add_char buf '='
  done;
  try Some (Base64.decode_exn (Buffer.contents buf)) with _ -> None

(* Check JWT claims only — no RS256 signature verification.
   See TEAMS_API.md for the known limitation note. *)
let check_jwt_claims ~(config : Runtime_config.teams_config) token =
  let parts = String.split_on_char '.' token in
  match parts with
  | [ _header; payload; _sig ] -> (
      match base64url_decode payload with
      | None -> Error "JWT payload base64url decode failed"
      | Some payload_json -> (
          try
            let json = Yojson.Safe.from_string payload_json in
            let open Yojson.Safe.Util in
            let aud = try json |> member "aud" |> to_string with _ -> "" in
            let iss = try json |> member "iss" |> to_string with _ -> "" in
            let exp = try json |> member "exp" |> to_number with _ -> 0.0 in
            let nbf = try json |> member "nbf" |> to_number with _ -> 0.0 in
            let now = Unix.gettimeofday () in
            if aud <> config.app_id then
              Error
                (Printf.sprintf
                   "JWT aud mismatch: got %s, expected %s — check that app_id \
                    in config matches the Application ID in Azure"
                   aud config.app_id)
            else if
              iss <> "https://api.botframework.com"
              && iss
                 <> Printf.sprintf "https://sts.windows.net/%s/"
                      config.tenant_id
            then
              Error
                (Printf.sprintf
                   "JWT iss not trusted: %s — request may not be from \
                    Microsoft Bot Framework"
                   iss)
            else if exp < now then Error "JWT expired — check server clock"
            else if nbf > now +. 300.0 then
              Error "JWT nbf in future — check server clock (NTP sync issue?)"
            else Ok ()
          with exn ->
            Error
              (Printf.sprintf "JWT parse error: %s" (Printexc.to_string exn))))
  | _ -> Error "JWT must have 3 parts"

(* Extract Bearer token from Authorization header value *)
let extract_bearer auth_header =
  let prefix = "Bearer " in
  let plen = String.length prefix in
  if String.length auth_header > plen && String.sub auth_header 0 plen = prefix
  then Some (String.sub auth_header plen (String.length auth_header - plen))
  else None

(* Verify inbound request Authorization header *)
let verify_auth ~(config : Runtime_config.teams_config) auth_header =
  match extract_bearer auth_header with
  | None ->
      Lwt.return (Error "Missing or malformed Authorization: Bearer header")
  | Some token -> Lwt.return (check_jwt_claims ~config token)

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
        if !cut = !pos then cut := limit;
        result := String.sub text !pos (!cut - !pos) :: !result;
        pos := !cut + 1
      end
    done;
    List.rev !result
  end

(* Build a reply activity JSON body, optionally with an @mention and
   notification alert control.
   Only include channelData.notification when suppressing (alert=false);
   omit it entirely for normal replies so Teams uses default behavior. *)
let build_reply_body ~alert ~text ~mention =
  let text_with_mention, entities =
    match mention with
    | Some { mention_id; mention_name } ->
        let at_tag = Printf.sprintf "<at>%s</at>" mention_name in
        let full_text = Printf.sprintf "%s %s" at_tag text in
        let entity =
          `Assoc
            [
              ("type", `String "mention");
              ( "mentioned",
                `Assoc
                  [ ("id", `String mention_id); ("name", `String mention_name) ]
              );
              ("text", `String at_tag);
            ]
        in
        (full_text, [ entity ])
    | None -> (text, [])
  in
  let base =
    [ ("type", `String "message"); ("text", `String text_with_mention) ]
  in
  let base =
    if alert then base
    else
      base
      @ [
          ( "channelData",
            `Assoc [ ("notification", `Assoc [ ("alert", `Bool false) ]) ] );
        ]
  in
  let fields =
    match entities with
    | [] -> base
    | ents -> base @ [ ("entities", `List ents) ]
  in
  `Assoc fields |> Yojson.Safe.to_string

(* Send a reply via Bot Framework REST API.
   ~alert controls channelData.notification.alert: true triggers a
   desktop/mobile notification toast, false suppresses it. *)
let send_reply ?(alert = false) ~(config : Runtime_config.teams_config)
    ~service_url ~conversation_id ~reply_to_id ~text ?mention () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot send reply, no OAuth token");
      Lwt.return_unit
  | Some token ->
      let chunks = split_message text in
      Lwt_list.iter_s
        (fun chunk ->
          let uri =
            if reply_to_id = "" then
              Printf.sprintf "%s/v3/conversations/%s/activities"
                (String.trim service_url)
                (Uri.pct_encode conversation_id)
            else
              Printf.sprintf "%s/v3/conversations/%s/activities/%s"
                (String.trim service_url)
                (Uri.pct_encode conversation_id)
                (Uri.pct_encode reply_to_id)
          in
          let headers = [ ("Authorization", "Bearer " ^ token) ] in
          let body = build_reply_body ~alert ~text:chunk ~mention in
          let* status, resp = Http_client.post_json ~uri ~headers ~body in
          if status < 200 || status >= 300 then
            Logs.warn (fun m ->
                m "Teams: send_reply failed (HTTP %d) conv=%s: %s" status
                  conversation_id resp);
          Lwt.return_unit)
        chunks

let is_team_allowed ~(config : Runtime_config.teams_config) ~team_id =
  match config.allow_teams with [ "*" ] -> true | ids -> List.mem team_id ids

let is_user_allowed ~(config : Runtime_config.teams_config) ~user_id =
  match config.allow_users with [ "*" ] -> true | ids -> List.mem user_id ids

(* Parse a Teams activity JSON body. Returns relevant fields or None if not a
   processable user message. *)
let parse_activity body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    let open Yojson.Safe.Util in
    let activity_type = try json |> member "type" |> to_string with _ -> "" in
    if activity_type <> "message" then None
    else
      let text = try json |> member "text" |> to_string with _ -> "" in
      let activity_id = try json |> member "id" |> to_string with _ -> "" in
      let service_url =
        try json |> member "serviceUrl" |> to_string with _ -> ""
      in
      let from_obj = try json |> member "from" with _ -> `Null in
      let user_id = try from_obj |> member "id" |> to_string with _ -> "" in
      let user_name =
        try from_obj |> member "name" |> to_string with _ -> ""
      in
      let conversation_obj =
        try json |> member "conversation" with _ -> `Null
      in
      let conversation_id =
        try conversation_obj |> member "id" |> to_string with _ -> ""
      in
      let is_group =
        try conversation_obj |> member "isGroup" |> to_bool with _ -> false
      in
      let team_id =
        try
          json |> member "channelData" |> member "team" |> member "id"
          |> to_string
        with _ -> ""
      in
      if text = "" || conversation_id = "" || user_id = "" then None
      else
        Some
          {
            activity_id;
            service_url;
            conversation_id;
            user_id;
            user_name;
            team_id;
            text;
            is_group;
          }
  with _ -> None

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
      if !j + 4 <= len then i := !j + 5 else i := len
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
    ~(session_manager : Session.t) ~auth_header body_str =
  let open Lwt.Syntax in
  (* Verify JWT claims *)
  let* auth_result = verify_auth ~config auth_header in
  match auth_result with
  | Error reason ->
      Logs.warn (fun m -> m "Teams: auth failed: %s" reason);
      Lwt.return_unit
  | Ok () -> (
      match parse_activity body_str with
      | None -> Lwt.return_unit
      | Some
          {
            activity_id;
            service_url;
            conversation_id;
            user_id;
            user_name;
            team_id;
            text = raw_text;
            is_group;
          } -> (
          if dedup_seen activity_id then Lwt.return_unit
          else
            let text = strip_at_mentions raw_text in
            if text = "" then Lwt.return_unit
            else
              let effective_team_id =
                if team_id = "" then "personal" else team_id
              in
              Logs.info (fun m ->
                  m "Teams: message from user=%s team=%s conv=%s" user_id
                    effective_team_id conversation_id);
              if not (is_team_allowed ~config ~team_id:effective_team_id) then (
                Logs.warn (fun m ->
                    m "Teams: ignoring message from unauthorized team=%s"
                      effective_team_id);
                Lwt.return_unit)
              else if not (is_user_allowed ~config ~user_id) then (
                Logs.warn (fun m ->
                    m "Teams: ignoring message from unauthorized user=%s"
                      user_id);
                Lwt.return_unit)
              else
                let effective_service_url =
                  if service_url = "" then config.service_url else service_url
                in
                let key =
                  session_key ~team_id:effective_team_id ~conversation_id
                in
                let sender_name =
                  if user_name = "" then None else Some user_name
                in
                (* @mention the sender in group chats so they get a
                   notification *)
                let mention =
                  if is_group && user_name <> "" then
                    Some { mention_id = user_id; mention_name = user_name }
                  else None
                in
                (* Register alerting notifier for ask_user_question *)
                Session.register_alert_channel_notifier session_manager ~key
                  (fun reply_text ->
                    send_reply ~alert:true ~config
                      ~service_url:effective_service_url ~conversation_id
                      ~reply_to_id:activity_id ~text:reply_text ?mention ());
                let* result =
                  Session.with_registered_notifier session_manager ~key
                    ~notify:(fun reply_text ->
                      send_reply ~alert:false ~config
                        ~service_url:effective_service_url ~conversation_id
                        ~reply_to_id:activity_id ~text:reply_text ?mention ())
                    (fun () ->
                      Lwt.catch
                        (fun () ->
                          let* response =
                            Session.turn session_manager ~key ~message:text
                              ~channel_name:"teams" ~channel_type:"webhook"
                              ~sender_id:user_id ?sender_name ()
                          in
                          Lwt.return (Ok response))
                        (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
                in
                match result with
                | Ok response ->
                    if Session.is_queued_message_response response then
                      Lwt.return_unit
                    else
                      send_reply ~alert:true ~config
                        ~service_url:effective_service_url ~conversation_id
                        ~reply_to_id:activity_id ~text:response ?mention ()
                | Error err ->
                    Logs.err (fun m ->
                        m "Teams: agent error for conv=%s user=%s: %s"
                          conversation_id user_id err);
                    Lwt.return_unit))

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
