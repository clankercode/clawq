open Telegram_api

type resolved = {
  user_text : string;
  image_content_parts : Provider.content_part list;
  doc_attachments : (string * string) list;
}

let empty user_text =
  { user_text; image_content_parts = []; doc_attachments = [] }

let voice_to_text ~bot_token ~(update : update) ~(session_mgr : Session.t) =
  let open Lwt.Syntax in
  match update.voice_file_id with
  | None -> Lwt.return_none
  | Some file_id -> (
      let config = Session.get_config session_mgr in
      match
        Voice_transcription.validate ~config ~filename:"voice.ogg"
          ~mime_type:(Some "audio/ogg") ~size:update.voice_file_size
          ~duration_seconds:update.voice_duration
      with
      | Error reason ->
          Logs.info (fun m ->
              m "Telegram voice skipped: %s"
                (Voice_transcription.skip_reason_to_string reason));
          Lwt.return (Some (empty ""))
      | Ok () ->
          let* text =
            Lwt.catch
              (fun () ->
                let get_file_uri =
                  Printf.sprintf "%s%s/getFile?file_id=%s" !api_base bot_token
                    file_id
                in
                let* _status, file_body =
                  Http_client.get ~uri:get_file_uri ~headers:[]
                in
                let file_json = Yojson.Safe.from_string file_body in
                let file_path =
                  Yojson.Safe.Util.(
                    file_json |> member "result" |> member "file_path"
                    |> to_string)
                in
                let download_uri =
                  Printf.sprintf "https://api.telegram.org/file/bot%s/%s"
                    bot_token file_path
                in
                let* _status, audio_data =
                  Http_client.get ~uri:download_uri ~headers:[]
                in
                let filename = Filename.basename file_path in
                let notifier =
                  Telegram_api.make_status_notifier ~bot_token
                    ~chat_id:update.chat_id
                in
                Voice_transcription.transcribe_with_progress ~config ~notifier
                  ~audio_data ~filename ())
              (fun exn ->
                Logs.err (fun m ->
                    m "Voice transcription failed: %s" (Printexc.to_string exn));
                Lwt.return "")
          in
          Lwt.return (Some (empty text)))

let image_file_id (update : update) =
  match update.photo_file_id with
  | Some fid -> Some fid
  | None -> (
      match update.sticker_file_id with
      | Some fid -> Some fid
      | None -> (
          match (update.document_file_id, update.document_mime_type) with
          | Some fid, Some mt
            when String.length mt >= 6 && String.sub mt 0 6 = "image/" ->
              Some fid
          | _ -> None))

let disabled_image_text (update : update) =
  let name =
    if update.photo_file_id <> None then "photo"
    else if update.sticker_file_id <> None then "sticker"
    else "image"
  in
  let cap = match update.caption with Some c -> " — " ^ c | None -> "" in
  Printf.sprintf "[Attachment: %s (download disabled)%s]" name cap

let resolve_image ~bot_token ~(update : update) ~(session_mgr : Session.t) ~key
    ~workspace ~downloads_enabled file_id =
  let open Lwt.Syntax in
  if not downloads_enabled then Lwt.return (empty (disabled_image_text update))
  else
    Lwt.catch
      (fun () ->
        let* image_data = download_telegram_file ~bot_token ~file_id in
        let media_type = Telegram_api.detect_mime_type image_data in
        let b64 = Base64.encode_exn image_data in
        let text =
          match update.caption with Some c -> c | None -> "[Image]"
        in
        let image_content_parts =
          [ Provider.Image_base64 { data = b64; media_type } ]
        in
        let ext =
          match media_type with
          | "image/png" -> ".png"
          | "image/gif" -> ".gif"
          | "image/webp" -> ".webp"
          | _ -> ".jpg"
        in
        let filename = "image" ^ ext in
        let path =
          Attachment_download.save_to_downloads ~workspace ~filename
            ~data:image_data
        in
        Logs.info (fun m ->
            m "telegram: saved image attachment (%s, %d bytes) -> %s" media_type
              (String.length image_data) path);
        (match Session.get_db session_mgr with
        | Some db ->
            Memory.log_attachment_download ~db ~session_key:key
              ~source:"telegram" ~filename ~mime_type:media_type
              ~size_bytes:(String.length image_data) ~saved_path:path
        | None -> ());
        Lwt.return
          {
            user_text = text;
            image_content_parts;
            doc_attachments = [ ("image", path) ];
          })
      (fun exn ->
        Logs.err (fun m ->
            m "Image download failed: %s" (Printexc.to_string exn));
        let cap =
          match update.caption with Some c -> " — " ^ c | None -> ""
        in
        if update.photo_file_id <> None then
          Lwt.return (empty ("[Photo received" ^ cap ^ "]"))
        else if update.sticker_file_id <> None then
          Lwt.return (empty ("[Sticker received" ^ cap ^ "]"))
        else Lwt.return (empty ("[Image document received" ^ cap ^ "]")))

let resolve_document ~bot_token ~(update : update) ~(session_mgr : Session.t)
    ~key ~workspace ~downloads_enabled file_id =
  let open Lwt.Syntax in
  if not downloads_enabled then
    let name =
      match update.document_name with Some n -> n | None -> "document"
    in
    Lwt.return
      (empty (Printf.sprintf "[Attachment: %s (download disabled)]" name))
  else
    Lwt.catch
      (fun () ->
        let* data = download_telegram_file ~bot_token ~file_id in
        let filename =
          match update.document_name with Some n -> n | None -> "document"
        in
        let mime_hint =
          match update.document_mime_type with Some m -> m | None -> ""
        in
        let result =
          Attachment_download.classify_downloaded ~data ~filename ~mime_hint
            ~workspace
        in
        (match Session.get_db session_mgr with
        | Some db ->
            let mime = Attachment_download.detect_mime_type data in
            let path =
              match result with
              | ImagePart { path; _ }
              | InlineText { path; _ }
              | SavedFile { path; _ } ->
                  path
              | Skipped _ -> ""
            in
            if path <> "" then
              Memory.log_attachment_download ~db ~session_key:key
                ~source:"telegram" ~filename ~mime_type:mime
                ~size_bytes:(String.length data) ~saved_path:path
        | None -> ());
        let cap = match update.caption with Some c -> c | None -> "" in
        match result with
        | Attachment_download.ImagePart { content_part; path } ->
            Logs.info (fun m ->
                m "telegram: downloaded attachment %s (%d bytes) -> %s" filename
                  (String.length data) path);
            Lwt.return
              {
                user_text =
                  (if cap <> "" then cap else "[Image: " ^ filename ^ "]");
                image_content_parts = [ content_part ];
                doc_attachments = [ ("image", path) ];
              }
        | Attachment_download.InlineText { filename = fn; content; path } ->
            Logs.info (fun m ->
                m "telegram: downloaded attachment %s (%d bytes) -> %s" fn
                  (String.length data) path);
            let prefix = if cap <> "" then cap ^ "\n" else "" in
            Lwt.return
              {
                user_text =
                  Printf.sprintf "%s[File: %s]\n```\n%s\n```" prefix fn content;
                image_content_parts = [];
                doc_attachments = [ ("text", path) ];
              }
        | Attachment_download.SavedFile { file_type; path } ->
            Logs.info (fun m ->
                m "telegram: downloaded attachment %s (%d bytes) -> %s" filename
                  (String.length data) path);
            Lwt.return
              {
                user_text =
                  (if cap <> "" then cap
                   else Printf.sprintf "[Attachment: %s]" filename);
                image_content_parts = [];
                doc_attachments = [ (file_type, path) ];
              }
        | Attachment_download.Skipped placeholder ->
            Lwt.return (empty placeholder))
      (fun exn ->
        Logs.err (fun m ->
            m "Telegram document download failed: %s" (Printexc.to_string exn));
        let name =
          match update.document_name with Some n -> ": " ^ n | None -> ""
        in
        let cap =
          match update.caption with Some c -> " — " ^ c | None -> ""
        in
        Lwt.return (empty ("[Document" ^ name ^ cap ^ "]")))

let resolve_user_text ~bot_token ~(update : update) ~(session_mgr : Session.t)
    ~key () =
  let open Lwt.Syntax in
  let config = Session.get_config session_mgr in
  let workspace = Runtime_config.effective_workspace config in
  let downloads_enabled = config.security.attachment_downloads_enabled in
  let* voice = voice_to_text ~bot_token ~update ~session_mgr in
  match voice with
  | Some resolved -> Lwt.return resolved
  | None -> (
      match image_file_id update with
      | Some file_id ->
          resolve_image ~bot_token ~update ~session_mgr ~key ~workspace
            ~downloads_enabled file_id
      | None -> (
          match update.document_file_id with
          | Some file_id ->
              resolve_document ~bot_token ~update ~session_mgr ~key ~workspace
                ~downloads_enabled file_id
          | None -> Lwt.return (empty update.text)))
