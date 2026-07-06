include Teams_api

let authorization_headers = function
  | Some tok -> [ ("Authorization", "Bearer " ^ tok) ]
  | None -> []

let transcribe_audio_attachment ~teams_config ~full_config ~service_url
    ~conversation_id ~reply_to_id ~headers
    (attachment : Teams_activity_parser.teams_attachment) =
  let open Lwt.Syntax in
  match
    Voice_transcription.validate ~config:full_config ~filename:attachment.name
      ~mime_type:(Some attachment.content_type) ~size:None
      ~duration_seconds:None
  with
  | Error reason ->
      Logs.info (fun m ->
          m "Teams voice skipped %s: %s" attachment.name
            (Voice_transcription.skip_reason_to_string reason));
      Lwt.return ""
  | Ok () ->
      Lwt.catch
        (fun () ->
          let* _status, audio_data =
            Http_client.get ~uri:attachment.content_url ~headers
          in
          let notifier =
            make_status_notifier ~config:teams_config ~service_url
              ~conversation_id ~reply_to_id
          in
          Voice_transcription.transcribe_with_progress ~config:full_config
            ~notifier ~audio_data ~filename:attachment.name ())
        (fun exn ->
          Logs.err (fun m ->
              m "Teams voice transcription failed %s: %s" attachment.name
                (Printexc.to_string exn));
          Lwt.return "")

let resolve_transcription_prefix ~teams_config ~(full_config : Runtime_config.t)
    ~service_url ~conversation_id ~reply_to_id ~attachment_token audio_atts =
  let open Lwt.Syntax in
  if audio_atts <> [] && full_config.security.attachment_downloads_enabled then
    let headers = authorization_headers attachment_token in
    let* texts =
      Lwt_list.map_s
        (transcribe_audio_attachment ~teams_config ~full_config ~service_url
           ~conversation_id ~reply_to_id ~headers)
        audio_atts
    in
    Lwt.return (String.concat "" (List.filter (fun s -> s <> "") texts))
  else Lwt.return ""

let placeholder_text ~message attachments =
  if attachments = [] then message
  else
    let names =
      List.map
        (fun (a : Teams_activity_parser.teams_attachment) ->
          Printf.sprintf "\n[Attachment: %s (download disabled)]" a.name)
        attachments
    in
    message ^ String.concat "" names

let process_non_audio_attachments ~session_manager ~key ~workspace ~headers
    ~message attachments =
  let metas =
    List.map
      (fun (a : Teams_activity_parser.teams_attachment) ->
        Attachment_download.
          {
            url = a.content_url;
            filename = a.name;
            mime_type = Some a.content_type;
            size = None;
          })
      attachments
  in
  Attachment_download.process_attachments metas ~headers ~workspace
    ~db:(Session.get_db session_manager)
    ~session_key:key ~source:"teams" ~content_parts:[] ~attachments:[] ~message

let resolve ~teams_config ~session_manager ~key ~service_url ~conversation_id
    ~reply_to_id ~text parsed_attachments =
  let open Lwt.Syntax in
  let full_config : Runtime_config.t = Session.get_config session_manager in
  let audio_atts, non_audio_atts =
    List.partition
      (fun (a : Teams_activity_parser.teams_attachment) ->
        Voice_transcription.is_audio_mime a.content_type)
      parsed_attachments
  in
  let needs_token =
    full_config.security.attachment_downloads_enabled
    && (audio_atts <> [] || non_audio_atts <> [])
  in
  let* attachment_token =
    if needs_token then fetch_token ~config:teams_config else Lwt.return None
  in
  let* transcription_prefix =
    resolve_transcription_prefix ~teams_config ~full_config ~service_url
      ~conversation_id ~reply_to_id ~attachment_token audio_atts
  in
  let message =
    if transcription_prefix <> "" then transcription_prefix ^ "\n" ^ text
    else text
  in
  if non_audio_atts <> [] && full_config.security.attachment_downloads_enabled
  then
    let headers = authorization_headers attachment_token in
    let workspace = Runtime_config.effective_workspace full_config in
    process_non_audio_attachments ~session_manager ~key ~workspace ~headers
      ~message non_audio_atts
  else Lwt.return ([], [], placeholder_text ~message non_audio_atts)
