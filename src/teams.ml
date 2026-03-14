(* MS Teams channel via Microsoft Bot Framework webhook *)
(* See src/TEAMS_API.md for protocol and auth details *)

let max_message_chars = 28672
let dedup = Channel_util.Lru_dedup.create 500

let session_key ~team_id ~conversation_id =
  Printf.sprintf "teams:%s:%s" team_id conversation_id

let encode_channel_id ~service_url ~conversation_id =
  service_url ^ "|" ^ conversation_id

let decode_channel_id channel_id =
  match String.split_on_char '|' channel_id with
  | service_url :: rest -> (service_url, String.concat "|" rest)
  | [] -> ("", channel_id)

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

(* Send a typing indicator via Bot Framework REST API.
   Posts a {"type":"typing"} activity to the conversation. *)
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
        if !cut = !pos then cut := limit;
        result := String.sub text !pos (!cut - !pos) :: !result;
        pos := !cut + 1
      end
    done;
    List.rev !result
  end

(* Build a reply activity JSON body, optionally with an @mention and
   notification alert control.
   Always include channelData.notification.alert explicitly: true forces a
   desktop/mobile toast notification, false suppresses it.
   mention_mode controls how the mention is rendered:
     "entity" (default): Teams <at>Name</at> with entity markup (mention badge).
     "text": plain @Name prefix, no entity (no mention badge).
     "none" or anything else: no prefix added. *)
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

(* Send a reply via Bot Framework REST API.
   ~alert controls channelData.notification.alert: true triggers a
   desktop/mobile notification toast, false suppresses it. *)
let send_reply ?(alert = false) ~(config : Runtime_config.teams_config)
    ~service_url ~conversation_id ~reply_to_id ~text ?mention () =
  let open Lwt.Syntax in
  if
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
              let body =
                build_reply_body ~alert ~text:chunk ~mention
                  ~mention_mode:config.mention_mode
              in
              let* status, resp = Http_client.post_json ~uri ~headers ~body in
              if status >= 200 && status < 300 then begin
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
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot edit activity, no OAuth token");
      Lwt.return_unit
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
      let* status, resp = Http_client.put_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then
        Logs.warn (fun m ->
            m "Teams: edit_activity failed (HTTP %d) conv=%s activity=%s: %s"
              status conversation_id activity_id resp);
      Lwt.return_unit

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
      let headers =
        Cohttp.Header.of_list [ ("Authorization", "Bearer " ^ token) ]
      in
      let uri_obj = Uri.of_string uri in
      let* resp, resp_body = Cohttp_lwt_unix.Client.delete ~headers uri_obj in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      if status < 200 || status >= 300 then begin
        let* body_str = Cohttp_lwt.Body.to_string resp_body in
        Logs.warn (fun m ->
            m "Teams: delete_activity failed (HTTP %d) conv=%s activity=%s: %s"
              status conversation_id activity_id body_str);
        Lwt.return_unit
      end
      else Lwt.return_unit

let send_adaptive_card ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~reply_to_id ~card () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Logs.err (fun m -> m "Teams: cannot send adaptive card, no OAuth token");
      Lwt.return_unit
  | Some token ->
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
      let body = Yojson.Safe.to_string card in
      let* status, resp = Http_client.post_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then
        Logs.warn (fun m ->
            m "Teams: send_adaptive_card failed (HTTP %d) conv=%s: %s" status
              conversation_id resp);
      Lwt.return_unit

(* Build JSON body for the Bot Framework attachment upload endpoint. *)
let build_attachment_upload_body ~filename ~content_type ~content =
  let encoded = Base64.encode_exn content in
  `Assoc
    [
      ("type", `String content_type);
      ("name", `String filename);
      ("originalBase64", `String encoded);
    ]
  |> Yojson.Safe.to_string

(* Build a message activity JSON body with a file attachment reference. *)
let build_message_with_attachment ~filename ~content_type ~content_url =
  `Assoc
    [
      ("type", `String "message");
      ("text", `String filename);
      ( "attachments",
        `List
          [
            `Assoc
              [
                ("contentType", `String content_type);
                ("contentUrl", `String content_url);
                ("name", `String filename);
              ];
          ] );
    ]
  |> Yojson.Safe.to_string

(* Upload an attachment to a conversation via Bot Framework REST API.
   Returns Ok content_url on success, Error msg on failure.
   NOTE: This endpoint only works for Direct Line and Web Chat channels.
   Teams returns HTTP 404 — use Temp_downloads for Teams file delivery. *)
let upload_attachment ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~filename ~content_type ~content () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None -> Lwt.return (Error "No OAuth token available")
  | Some token ->
      let uri =
        Printf.sprintf "%s/v3/conversations/%s/attachments"
          (String.trim service_url)
          (Uri.pct_encode conversation_id)
      in
      let headers = [ ("Authorization", "Bearer " ^ token) ] in
      let body =
        build_attachment_upload_body ~filename ~content_type ~content
      in
      let* status, resp = Http_client.post_json ~uri ~headers ~body in
      if status >= 200 && status < 300 then
        try
          let json = Yojson.Safe.from_string resp in
          let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
          let content_url =
            Printf.sprintf "%s/v3/attachments/%s/views/original"
              (String.trim service_url) (Uri.pct_encode id)
          in
          Logs.info (fun m ->
              m "Teams: uploaded attachment %s conv=%s" filename conversation_id);
          Lwt.return (Ok content_url)
        with exn ->
          Lwt.return
            (Error
               (Printf.sprintf "Failed to parse upload response: %s"
                  (Printexc.to_string exn)))
      else
        Lwt.return
          (Error (Printf.sprintf "Upload failed (HTTP %d): %s" status resp))

(* Send a file as an attachment in a Teams message.
   Uploads via Bot Framework attachment API, then sends a message
   referencing the uploaded content URL. *)
let send_file ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~reply_to_id ~filename ~content ~content_type () =
  let open Lwt.Syntax in
  let* upload_result =
    upload_attachment ~config ~service_url ~conversation_id ~filename
      ~content_type ~content ()
  in
  match upload_result with
  | Error msg -> Lwt.return (Error msg)
  | Ok content_url -> (
      let* token_opt = fetch_token ~config in
      match token_opt with
      | None -> Lwt.return (Error "No OAuth token available for send")
      | Some token ->
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
          let body =
            build_message_with_attachment ~filename ~content_type ~content_url
          in
          let* status, resp = Http_client.post_json ~uri ~headers ~body in
          if status >= 200 && status < 300 then Lwt.return (Ok ())
          else
            Lwt.return
              (Error
                 (Printf.sprintf "Send with attachment failed (HTTP %d): %s"
                    status resp)))

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
                  m "Teams: message from user=%s (id=%s) team=%s conv=%s"
                    (if user_name <> "" then user_name else user_id)
                    user_id effective_team_id conversation_id);
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
                  session_key ~team_id:effective_team_id ~conversation_id
                in
                let sender_name =
                  if user_name = "" then None else Some user_name
                in
                (* @mention the sender in group chats so they get a
                   notification. Only on final responses and ask_user_question
                   prompts — not on intermediate streaming updates (notify). *)
                let mention =
                  if
                    is_group && user_name <> "" && config.mention_mode <> "none"
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
                (* Ensure a typing indicator watcher is running for this
                   session. The watcher tracks Session live_activity and
                   sends typing activities while the session is active. *)
                let typing_watcher =
                  Typing_indicator.ensure_session_typing_watcher
                    ~session_mgr:session_manager ~key
                    ~send_action:(fun () ->
                      send_typing_activity ~config
                        ~service_url:effective_service_url ~conversation_id)
                    ~interval:3.0 ~idle_timeout:300.0
                in
                let refresh_typing () = typing_watcher.refresh () in
                match Slash_commands.handle text with
                | NotACommand -> (
                    (* Register status message factory and capabilities *)
                    if
                      Option.is_none
                        (Session.find_connector_capabilities session_manager
                           ~key)
                    then begin
                      Session.register_connector_capabilities session_manager
                        ~key Connector_capabilities.teams;
                      Session.register_status_message_factory session_manager
                        ~key (fun () ->
                          let notifier =
                            make_status_notifier ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:activity_id
                          in
                          Status_message.create ~notifier ~parse_mode:"Teams" ())
                    end;
                    (* Register alerting notifier for ask_user_question *)
                    Session.register_alert_channel_notifier session_manager ~key
                      (fun reply_text ->
                        let* _id =
                          send_reply ~alert:true ~config
                            ~service_url:effective_service_url ~conversation_id
                            ~reply_to_id:activity_id ~text:reply_text ?mention
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit);
                    let* result =
                      Session.with_registered_notifier session_manager ~key
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
                              let* response =
                                Session.turn session_manager ~key ~message:text
                                  ~channel_name:"teams" ~channel_type:"webhook"
                                  ~channel:"teams"
                                  ~channel_id:
                                    (encode_channel_id
                                       ~service_url:effective_service_url
                                       ~conversation_id)
                                  ~sender_id:user_id ?sender_name ()
                              in
                              Lwt.return (Ok response))
                            (fun exn ->
                              Lwt.return (Error (Printexc.to_string exn))))
                    in
                    match result with
                    | Ok response ->
                        if Session.is_queued_message_response response then
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
                              "Teams: agent error for conv=%s user=%s (id=%s): \
                               %s"
                              conversation_id
                              (if user_name <> "" then user_name else user_id)
                              user_id err);
                        Lwt.return_unit)
                | Reply text -> send_text text
                | Help ->
                    let text =
                      Slash_commands.format_help ~connector:Format_adapter.Teams
                    in
                    send_text text
                | Menu page ->
                    let card_json =
                      Slash_commands_manifest.menu_adaptive_card_json ~page ()
                    in
                    send_adaptive_card ~config
                      ~service_url:effective_service_url ~conversation_id
                      ~reply_to_id:activity_id ~card:card_json ()
                | Reset ->
                    let* active_bg_tasks = Session.reset session_manager ~key in
                    send_text (Slash_commands.reset_message ~active_bg_tasks ())
                | Compact -> (
                    let* compact_result =
                      Session.compact session_manager ~key ()
                    in
                    match compact_result with
                    | Ok _ -> Lwt.return_unit
                    | Error err ->
                        send_text (Printf.sprintf "Compaction failed: %s" err))
                | RuntimeCtx ->
                    let* text =
                      Session.runtime_context_block session_manager ~key
                    in
                    send_text text
                | Uptime ->
                    send_text
                      (Daemon_status.daemon_uptime_reply
                         ~pid:(Daemon_status.read_current_daemon_pid ()))
                | Status ->
                    let text =
                      Slash_commands.format_status
                        ~connector:Format_adapter.Teams
                        ~db:(Session.get_db session_manager)
                        ~session_count:(Session.session_count session_manager)
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
                      (Printf.sprintf "Current thinking level: %s"
                         (Slash_commands.thinking_level_to_string current))
                | Thinking (Slash_commands.SetThinking level) ->
                    let cfg = Session.get_config session_manager in
                    let text =
                      match Config_set.set_reasoning_effort level with
                      | Ok () ->
                          Session.update_config ~source:"teams" session_manager
                            {
                              cfg with
                              agent_defaults =
                                {
                                  cfg.agent_defaults with
                                  reasoning_effort = level;
                                };
                            };
                          Printf.sprintf "Thinking set to: %s"
                            (Slash_commands.thinking_level_to_string level)
                      | Error err -> "Failed to set thinking level: " ^ err
                    in
                    send_text text
                | ShowThinking action ->
                    let cfg = Session.get_config session_manager in
                    let current = cfg.agent_defaults.show_thinking in
                    let text =
                      match action with
                      | Slash_commands.ShowThinkingStatus ->
                          Printf.sprintf "Show thinking: %s"
                            (if current then "on" else "off")
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
                              Printf.sprintf "Show thinking: %s"
                                (if new_val then "on" else "off")
                          | Error err ->
                              "Failed to update show_thinking: " ^ err)
                    in
                    send_text text
                | Heartbeat action ->
                    let text =
                      match action with
                      | Slash_commands.HeartbeatStatus ->
                          Session.session_heartbeat_status_text session_manager
                            ~key
                      | Slash_commands.SetHeartbeat enabled -> (
                          match
                            Session.set_session_heartbeat session_manager ~key
                              ~enabled
                          with
                          | Ok () ->
                              Printf.sprintf "Heartbeat %s for session %s"
                                (if enabled then "enabled" else "disabled")
                                key
                          | Error err -> err)
                    in
                    send_text text
                | Delegate prompt ->
                    let* () =
                      send_text "Delegating to a temporary session..."
                    in
                    Session.delegate_turn session_manager ~prompt
                      ~send_reply:send_text;
                    Lwt.return_unit
                | ForkAnd prompt ->
                    let* () = send_text "Forking session..." in
                    Session.fork_and_run session_manager ~parent_key:key ~prompt
                      ~send_reply:send_text;
                    Lwt.return_unit
                | DebugDumpChat ->
                    let content = Session.dump_json session_manager ~key in
                    let timestamp =
                      Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
                    in
                    let safe_key =
                      String.map
                        (fun c ->
                          match c with
                          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> c
                          | _ -> '_')
                        key
                    in
                    let filename =
                      Printf.sprintf "session_%s_%d.json" safe_key timestamp
                    in
                    let token =
                      Temp_downloads.add ~content
                        ~content_type:"application/json" ~filename ~ttl_s:3600.0
                    in
                    let msg =
                      match Temp_downloads.download_url token with
                      | Some url ->
                          Printf.sprintf
                            "Session dump available for download (%d bytes, \
                             expires in 1 hour):\n\n\
                             %s"
                            (String.length content) url
                      | None ->
                          let max_len = 25000 in
                          if String.length content <= max_len then content
                          else
                            Printf.sprintf
                              "Session dump (truncated — configure tunnel.url \
                               for full file download):\n\
                               %s\n\
                               ...\n\n\
                               Full dump: %d bytes"
                              (String.sub content 0 max_len)
                              (String.length content)
                    in
                    send_text msg
                | Tools ->
                    let text =
                      match Session.get_tool_registry session_manager with
                      | Some reg ->
                          let tools, skills =
                            Tool_registry.partition_skills reg
                          in
                          Slash_commands.format_tools
                            ~connector:Format_adapter.Teams tools skills
                      | None -> "Tools are not enabled."
                    in
                    send_text text
                | Tasks ->
                    let text =
                      match Session.get_db session_manager with
                      | Some db ->
                          Task_tree.init_schema db;
                          Task_tree.render_emoji_tree ~db ~session_key:key ()
                      | None -> "Tasks are not available (no database)."
                    in
                    send_text text
                | TasksFull ->
                    let text =
                      match Session.get_db session_manager with
                      | Some db ->
                          Task_tree.init_schema db;
                          Task_tree.render_tree_with_legend ~db ~session_key:key
                      | None -> "Tasks are not available (no database)."
                    in
                    send_text text
                | Costs action ->
                    let text =
                      match Session.get_db session_manager with
                      | Some db ->
                          Slash_commands.format_costs
                            ~connector:Format_adapter.Teams ~db action
                      | None -> "Costs are not available (no database)."
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
                | Model action -> (
                    let open Slash_commands in
                    match action with
                    | ModelShow ->
                        let current =
                          Session.get_session_effective_model session_manager
                            ~key
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
                          format_model_show ~connector:Format_adapter.Teams
                            ~current ~favorites:prefs.favorites ~usage_ranked
                        in
                        send_text text
                    | ModelSet name -> (
                        let provider, model_id, fmt =
                          Models_catalog.split_name name
                        in
                        match fmt with
                        | Models_catalog.Canonical | Models_catalog.Legacy ->
                            let hint =
                              match fmt with
                              | Models_catalog.Legacy ->
                                  Printf.sprintf
                                    "\nHint: use %s:%s format instead of %s/%s."
                                    provider model_id provider model_id
                              | _ -> ""
                            in
                            let cfg = Session.get_config session_manager in
                            let provider_in_config =
                              List.mem_assoc provider cfg.providers
                            in
                            let warn =
                              if not provider_in_config then
                                Printf.sprintf
                                  "\n\
                                   Warning: provider '%s' not found in config. \
                                   Add it to your config.json to use this \
                                   model."
                                  provider
                              else ""
                            in
                            Session.set_session_model session_manager ~key
                              ~model:name;
                            send_text
                              (Printf.sprintf
                                 "Model set to: %s (provider: %s)%s%s\n\
                                  Persisted for this session across restarts. \
                                  Use /model set-default to change the global \
                                  default."
                                 model_id provider hint warn)
                        | Models_catalog.Plain -> (
                            let model_info =
                              Models_catalog.find_by_full_name name
                            in
                            match model_info with
                            | None ->
                                Session.set_session_model session_manager ~key
                                  ~model:name;
                                send_text
                                  (Printf.sprintf
                                     "Warning: '%s' not found in model \
                                      catalog. Setting anyway.\n\
                                      Persisted for this session across \
                                      restarts. Use /model set-default to \
                                      change the global default."
                                     name)
                            | Some m ->
                                Session.set_session_model session_manager ~key
                                  ~model:name;
                                let display =
                                  if m.Models_catalog.provider <> "" then
                                    Printf.sprintf
                                      "Model set to: %s (provider: %s)\n\
                                       Persisted for this session across \
                                       restarts. Use /model set-default to \
                                       change the global default."
                                      m.Models_catalog.id
                                      m.Models_catalog.provider
                                  else
                                    Printf.sprintf
                                      "Model set to: %s\n\
                                       Persisted for this session across \
                                       restarts. Use /model set-default to \
                                       change the global default."
                                      name
                                in
                                send_text display))
                    | ModelSetDefault name -> (
                        let provider, model_id, fmt =
                          Models_catalog.split_name name
                        in
                        let hint =
                          match fmt with
                          | Models_catalog.Legacy ->
                              Printf.sprintf "\nHint: use %s:%s format instead."
                                provider model_id
                          | _ -> ""
                        in
                        let result =
                          Config_set.set_json_value
                            "agent_defaults.primary_model" (`String name)
                        in
                        match result with
                        | Error e ->
                            send_text
                              (Printf.sprintf "Error writing config: %s" e)
                        | Ok () ->
                            let reply_text =
                              match fmt with
                              | Models_catalog.Canonical | Models_catalog.Legacy
                                ->
                                  Printf.sprintf
                                    "Default model set to: %s (provider: %s)%s\n\
                                     Applies to new sessions."
                                    model_id provider hint
                              | Models_catalog.Plain ->
                                  Printf.sprintf
                                    "Default model set to: %s\n\
                                     Applies to new sessions."
                                    name
                            in
                            send_text reply_text)
                    | ModelFav name ->
                        let prefs = Model_preferences.toggle_favorite name in
                        let status =
                          if List.mem name prefs.favorites then "added to"
                          else "removed from"
                        in
                        send_text (Printf.sprintf "%s %s favorites" name status)
                    | ModelUnfav name ->
                        let _ = Model_preferences.remove_favorite name in
                        send_text
                          (Printf.sprintf "Removed from favorites: %s" name)
                    | ModelList provider ->
                        let db_extras =
                          match Session.get_db session_manager with
                          | None -> []
                          | Some db ->
                              Model_discovery.get_db_only_models ~db
                                ~provider_filter:provider
                        in
                        let models =
                          Models_catalog.to_plain_list ~provider_filter:provider
                            ~db_extras ()
                          |> String.split_on_char '\n'
                          |> List.filter (fun s -> s <> "")
                        in
                        let text =
                          format_model_list ~connector:Format_adapter.Teams
                            ~models ~provider
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
                            ~connector:Format_adapter.Teams ~config:cfg results
                        in
                        send_text text)))

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
