(** Teams file upload support.

    Provides functions for uploading and sending files in Teams conversations.
    Separated from teams.ml to keep file size within limits. *)

(** Build JSON body for the Bot Framework attachment upload endpoint. *)
let build_attachment_upload_body ~filename ~content_type ~content =
  let encoded = Base64.encode_exn content in
  `Assoc
    [
      ("type", `String content_type);
      ("name", `String filename);
      ("originalBase64", `String encoded);
    ]
  |> Yojson.Safe.to_string

(** Build a message activity JSON body with a file attachment reference. *)
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

(** Upload an attachment to a conversation via Bot Framework REST API. Returns
    Ok content_url on success, Error msg on failure. NOTE: This endpoint only
    works for Direct Line and Web Chat channels. Teams returns HTTP 404 — use
    Temp_downloads for Teams file delivery. *)
let upload_attachment ~(config : Runtime_config.teams_config) ~service_url
    ~conversation_id ~filename ~content_type ~content () =
  let open Lwt.Syntax in
  let* token_opt = Teams_auth.fetch_token ~config in
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

(** Send a file as an attachment in a Teams message. Uploads via Bot Framework
    attachment API, then sends a message referencing the uploaded content URL.
*)
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
      let* token_opt = Teams_auth.fetch_token ~config in
      match token_opt with
      | None -> Lwt.return (Error "No OAuth token available for send")
      | Some token ->
          let uri =
            Teams.build_reply_uri ~service_url ~conversation_id ~reply_to_id
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
