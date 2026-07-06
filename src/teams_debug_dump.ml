include Teams_api

let safe_session_key key =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') as c -> c | _ -> '_')
    key

let send_temp_download ~send_text ~content ~filename =
  let token =
    Temp_downloads.add ~content ~content_type:"application/json" ~filename
      ~ttl_s:3600.0
  in
  let msg =
    match Temp_downloads.download_url token with
    | Some url ->
        Printf.sprintf
          "Session dump available for download (%d bytes, expires in 1 hour):\n\n\
           %s"
          (String.length content) url
    | None ->
        let max_len = 25000 in
        if String.length content <= max_len then content
        else
          Printf.sprintf
            "Session dump (truncated — configure tunnel.url for full file \
             download):\n\
             %s\n\
             ...\n\n\
             Full dump: %d bytes"
            (String.sub content 0 max_len)
            (String.length content)
  in
  send_text msg

let handle ~(session_manager : Session.t) ~key
    ~(config : Runtime_config.teams_config) ~service_url ~conversation_id
    ~reply_to_id ~team_id ~is_group ~user_group ~send_text =
  let open Lwt.Syntax in
  let content = Session.dump_json session_manager ~key in
  let timestamp = Int64.to_int (Int64.of_float (Unix.gettimeofday ())) in
  let filename =
    Printf.sprintf "session_%s_%d.json" (safe_session_key key) timestamp
  in
  let send_temp_download () =
    send_temp_download ~send_text ~content ~filename
  in
  let delivery =
    select_file_upload_delivery ~file_consent_cards:config.file_consent_cards
      ~team_id ~is_group
  in
  match delivery with
  | File_consent_card -> (
      let size_bytes = String.length content in
      let room_context =
        consent_room_context ~session_manager ~conversation_id ~user_group
          ?access_snapshot_id:None ()
      in
      let consent_id =
        store_pending_consent ?room_context ~content ~filename
          ~content_type:"application/json" ~ttl_s:600.0 ()
      in
      let* result =
        send_file_consent_card ?room_context ~config ~service_url
          ~conversation_id ~reply_to_id ~filename
          ~description:"Session debug dump" ~size_bytes ~consent_id ()
      in
      match result with
      | Ok () -> send_temp_download ()
      | Error err ->
          Logs.warn (fun m ->
              m
                "Teams: file consent card failed (%s), falling back to temp \
                 download"
                err);
          send_temp_download ())
  | Temp_download_url -> send_temp_download ()
