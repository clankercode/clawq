(* MS Teams file consent card flow (OneDrive upload) *)

type consent_room_context = {
  room_id : string;
  session_key : string;
  profile_name : string;
  user_group : string;
      (** "admin" or "guest" — preserved from the requester's policy at card
          creation time so the background completion uses the same access tier.
      *)
  access_snapshot_id : string option;
      (** Effective-access snapshot ID captured when the consent card was built.
          Lets the background upload completion continue with the original
          policy even after config changes. *)
}

type pending_consent = {
  content : string;
  filename : string;
  content_type : string;
  expires_at : float;
  room_context : consent_room_context option;
}

type file_upload_delivery = File_consent_card | Temp_download_url
type invoke_response = { status_code : int; body_json : Yojson.Safe.t }

let make_invoke_response ?(body = `Assoc []) status_code =
  {
    status_code;
    body_json = `Assoc [ ("status", `Int status_code); ("body", body) ];
  }

let ok_invoke_response () = make_invoke_response 200
let unauthorized_invoke_response () = make_invoke_response 401

let invoke_response_body (response : invoke_response) =
  Yojson.Safe.to_string response.body_json

let select_file_upload_delivery ~file_consent_cards ~team_id ~is_group =
  match (file_consent_cards, team_id, is_group) with
  | true, "", false -> File_consent_card
  | _ -> Temp_download_url

let pending_consents : (string, pending_consent) Hashtbl.t = Hashtbl.create 16

let generate_consent_id () =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate 16 in
  let buf = Buffer.create 32 in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let store_pending_consent ?room_context ~content ~filename ~content_type ~ttl_s
    () =
  let consent_id = generate_consent_id () in
  let entry =
    {
      content;
      filename;
      content_type;
      expires_at = Unix.gettimeofday () +. ttl_s;
      room_context;
    }
  in
  Hashtbl.replace pending_consents consent_id entry;
  consent_id

let get_pending_consent consent_id =
  match Hashtbl.find_opt pending_consents consent_id with
  | None -> None
  | Some entry ->
      if Unix.gettimeofday () > entry.expires_at then begin
        Hashtbl.remove pending_consents consent_id;
        None
      end
      else begin
        Hashtbl.remove pending_consents consent_id;
        Some entry
      end

let cleanup_pending_consents () =
  let now = Unix.gettimeofday () in
  let expired =
    Hashtbl.fold
      (fun k v acc -> if now > v.expires_at then k :: acc else acc)
      pending_consents []
  in
  List.iter (Hashtbl.remove pending_consents) expired

let consent_room_context_of_json json =
  let open Yojson.Safe.Util in
  let string_field name =
    try
      let value = json |> member name |> to_string in
      if value = "" then None else Some value
    with _ -> None
  in
  match
    ( string_field "roomId",
      string_field "sessionKey",
      string_field "roomProfileName" )
  with
  | Some room_id, Some session_key, Some profile_name ->
      let user_group =
        Option.value (string_field "userGroup") ~default:"guest"
      in
      let access_snapshot_id = string_field "accessSnapshotId" in
      Some
        { room_id; session_key; profile_name; user_group; access_snapshot_id }
  | _ -> None

let consent_context_json ?room_context ~consent_id () =
  let room_fields =
    match room_context with
    | None -> []
    | Some ctx -> (
        let base =
          [
            ("roomId", `String ctx.room_id);
            ("sessionKey", `String ctx.session_key);
            ("roomProfileName", `String ctx.profile_name);
            ("userGroup", `String ctx.user_group);
          ]
        in
        match ctx.access_snapshot_id with
        | Some sid -> ("accessSnapshotId", `String sid) :: base
        | None -> base)
  in
  `Assoc (("consentId", `String consent_id) :: room_fields)

let file_consent_description ?room_context description =
  match room_context with
  | Some { profile_name; user_group; _ } when String.trim profile_name <> "" ->
      Printf.sprintf "%s\nRoom profile: %s (%s)" description profile_name
        user_group
  | _ -> description

let build_file_consent_card ?room_context ~filename ~description ~size_bytes
    ~consent_id () =
  let ctx = consent_context_json ?room_context ~consent_id () in
  let description = file_consent_description ?room_context description in
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.teams.card.file.consent" );
                ("name", `String filename);
                ( "content",
                  `Assoc
                    [
                      ("description", `String description);
                      ("sizeInBytes", `Int size_bytes);
                      ("acceptContext", ctx);
                      ("declineContext", ctx);
                    ] );
              ];
          ] );
    ]
  |> Yojson.Safe.to_string

let build_file_info_card ~filename ~content_url ~unique_id ~file_type =
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.teams.card.file.info" );
                ("contentUrl", `String content_url);
                ("name", `String filename);
                ( "content",
                  `Assoc
                    [
                      ("uniqueId", `String unique_id);
                      ("fileType", `String file_type);
                    ] );
              ];
          ] );
    ]
  |> Yojson.Safe.to_string

let upload_to_onedrive ~upload_url ~content ~content_type =
  let open Lwt.Syntax in
  let content_length = String.length content in
  let range =
    Printf.sprintf "bytes 0-%d/%d" (content_length - 1) content_length
  in
  let headers =
    [
      ("Content-Range", range); ("Content-Length", string_of_int content_length);
    ]
  in
  let* status, resp =
    Http_client.put_raw ~uri:upload_url ~headers ~content_type ~body:content
  in
  if status >= 200 && status < 300 then (
    Logs.info (fun m -> m "Teams: OneDrive upload succeeded (HTTP %d)" status);
    Lwt.return (Ok ()))
  else (
    Logs.warn (fun m ->
        m "Teams: OneDrive upload failed (HTTP %d): %s" status resp);
    Lwt.return
      (Error (Printf.sprintf "OneDrive upload failed (HTTP %d)" status)))

(* Send a file consent card. Takes ~fetch_token and ~post_json_throttled
   as parameters to avoid circular module dependency. *)
let send_file_consent_card ?room_context ~fetch_token ~post_json_throttled
    ~(config : Runtime_config.teams_config) ~service_url ~conversation_id
    ~reply_to_id ~filename ~description ~size_bytes ~consent_id () =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None -> Lwt.return (Error "No OAuth token available")
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
        build_file_consent_card ?room_context ~filename ~description ~size_bytes
          ~consent_id ()
      in
      let* status, resp =
        post_json_throttled ~conversation_id ~uri ~headers ~body
      in
      if status >= 200 && status < 300 then Lwt.return (Ok ())
      else
        Lwt.return
          (Error
             (Printf.sprintf "FileConsentCard send failed (HTTP %d): %s" status
                resp))

(* Send a file info card after OneDrive upload. *)
let send_file_info_card ~fetch_token ~post_json_throttled
    ~(config : Runtime_config.teams_config) ~service_url ~conversation_id
    ~filename ~content_url ~unique_id ~file_type () =
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
        build_file_info_card ~filename ~content_url ~unique_id ~file_type
      in
      let* status, _resp =
        post_json_throttled ~conversation_id ~uri ~headers ~body
      in
      if status < 200 || status >= 300 then
        Logs.warn (fun m ->
            m "Teams: FileInfoCard send failed (HTTP %d) conv=%s" status
              conversation_id);
      Lwt.return_unit

(* Handle fileConsent/invoke activities. Returns the invoke response
   immediately — Teams requires a fast HTTP 200 reply. OneDrive upload and
   FileInfoCard delivery run in the background via Lwt.async. *)
let handle_file_consent_invoke ~fetch_token ~post_json_throttled ~send_reply
    ~(config : Runtime_config.teams_config) json =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let value = try json |> member "value" with _ -> `Null in
  let action = try value |> member "action" |> to_string with _ -> "" in
  let context = try value |> member "context" with _ -> `Null in
  let room_context_from_card = consent_room_context_of_json context in
  let consent_id =
    try context |> member "consentId" |> to_string with _ -> ""
  in
  let service_url =
    try json |> member "serviceUrl" |> to_string with _ -> ""
  in
  let conversation_id =
    try json |> member "conversation" |> member "id" |> to_string with _ -> ""
  in
  let effective_service_url =
    if service_url = "" then config.service_url else service_url
  in
  Logs.info (fun m ->
      m "Teams: file consent invoke action=%s consent_id=%s conv=%s" action
        consent_id conversation_id);
  if consent_id = "" then (
    Logs.warn (fun m -> m "Teams: file consent invoke with no consentId");
    Lwt.return (ok_invoke_response ()))
  else
    match action with
    | "accept" -> (
        match get_pending_consent consent_id with
        | None ->
            Logs.warn (fun m ->
                m "Teams: file consent accepted but no pending data for id=%s"
                  consent_id);
            (* Send error message in background — don't block invoke response *)
            Lwt.async (fun () ->
                Lwt.catch
                  (fun () ->
                    let* _id =
                      send_reply ~alert:false ~config
                        ~service_url:effective_service_url ~conversation_id
                        ~reply_to_id:""
                        ~text:"File consent expired or already processed." ()
                    in
                    Lwt.return_unit)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Teams: file consent error reply failed: %s"
                          (Printexc.to_string exn));
                    Lwt.return_unit));
            Lwt.return (ok_invoke_response ())
        | Some pending ->
            let room_context =
              match pending.room_context with
              | Some _ as ctx -> ctx
              | None -> room_context_from_card
            in
            (match room_context with
            | Some ctx ->
                Logs.info (fun m ->
                    m
                      "Teams: file consent accept preserved room context \
                       session=%s profile=%s user_group=%s snapshot=%s"
                      ctx.session_key ctx.profile_name ctx.user_group
                      (Option.value ctx.access_snapshot_id ~default:"none"))
            | None -> ());
            let upload_info =
              try value |> member "uploadInfo" with _ -> `Null
            in
            let upload_url =
              try upload_info |> member "uploadUrl" |> to_string with _ -> ""
            in
            let content_url =
              try upload_info |> member "contentUrl" |> to_string with _ -> ""
            in
            let unique_id =
              try upload_info |> member "uniqueId" |> to_string with _ -> ""
            in
            let file_type =
              try upload_info |> member "fileType" |> to_string with _ -> ""
            in
            if upload_url = "" then (
              Logs.warn (fun m ->
                  m "Teams: file consent accepted but no uploadUrl");
              Lwt.return (ok_invoke_response ()))
            else begin
              (* Upload to OneDrive and send FileInfoCard in background —
                 Teams requires a fast HTTP 200 invoke response *)
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* upload_result =
                        upload_to_onedrive ~upload_url ~content:pending.content
                          ~content_type:pending.content_type
                      in
                      match upload_result with
                      | Ok () ->
                          let* () =
                            send_file_info_card ~fetch_token
                              ~post_json_throttled ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~filename:pending.filename
                              ~content_url ~unique_id ~file_type ()
                          in
                          Logs.info (fun m ->
                              m
                                "Teams: file uploaded to OneDrive for \
                                 consent_id=%s conv=%s"
                                consent_id conversation_id);
                          Lwt.return_unit
                      | Error err ->
                          Logs.warn (fun m ->
                              m
                                "Teams: OneDrive upload failed for \
                                 consent_id=%s: %s"
                                consent_id err);
                          let* _id =
                            send_reply ~alert:false ~config
                              ~service_url:effective_service_url
                              ~conversation_id ~reply_to_id:""
                              ~text:
                                (Printf.sprintf
                                   "File upload to OneDrive failed: %s" err)
                              ()
                          in
                          Lwt.return_unit)
                    (fun exn ->
                      Logs.err (fun m ->
                          m "Teams: file consent upload background error: %s"
                            (Printexc.to_string exn));
                      Lwt.return_unit));
              Lwt.return (ok_invoke_response ())
            end)
    | "decline" ->
        let room_context =
          match get_pending_consent consent_id with
          | Some pending -> (
              match pending.room_context with
              | Some _ as ctx -> ctx
              | None -> room_context_from_card)
          | None -> room_context_from_card
        in
        (match room_context with
        | Some ctx ->
            Logs.info (fun m ->
                m
                  "Teams: file consent declined for id=%s session=%s \
                   profile=%s user_group=%s snapshot=%s"
                  consent_id ctx.session_key ctx.profile_name ctx.user_group
                  (Option.value ctx.access_snapshot_id ~default:"none"))
        | None ->
            Logs.info (fun m ->
                m "Teams: file consent declined for id=%s" consent_id));
        Lwt.return (ok_invoke_response ())
    | _ ->
        Logs.warn (fun m -> m "Teams: unknown file consent action=%s" action);
        Lwt.return (ok_invoke_response ())

(* Handle invoke activities from Teams. Returns (status_code, response_body).
   Called synchronously from http_server — caller responds with this status. *)
let handle_invoke ~fetch_token ~post_json_throttled ~send_reply
    ~(config : Runtime_config.teams_config) ~auth_header body_str =
  let open Lwt.Syntax in
  let* auth_result = Teams_auth.verify_auth ~config auth_header in
  match auth_result with
  | Error reason ->
      Logs.warn (fun m -> m "Teams: invoke auth failed: %s" reason);
      let response = unauthorized_invoke_response () in
      Lwt.return (response.status_code, invoke_response_body response)
  | Ok () -> (
      try
        let json = Yojson.Safe.from_string body_str in
        let name =
          try Yojson.Safe.Util.(json |> member "name" |> to_string)
          with _ -> ""
        in
        match name with
        | "fileConsent/invoke" ->
            let* response =
              handle_file_consent_invoke ~fetch_token ~post_json_throttled
                ~send_reply ~config json
            in
            Lwt.return (response.status_code, invoke_response_body response)
        | _ ->
            Logs.debug (fun m -> m "Teams: unhandled invoke name=%s" name);
            let response = ok_invoke_response () in
            Lwt.return (response.status_code, invoke_response_body response)
      with exn ->
        Logs.err (fun m ->
            m "Teams: invoke handler error: %s" (Printexc.to_string exn));
        let response = ok_invoke_response () in
        Lwt.return (response.status_code, invoke_response_body response))
