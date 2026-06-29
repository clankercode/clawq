type transcription_response = { text : string }

let content_type_of_ext filename =
  let ext = Filename.extension filename |> String.lowercase_ascii in
  match ext with
  | ".ogg" -> "audio/ogg"
  | ".mp3" -> "audio/mpeg"
  | ".wav" -> "audio/wav"
  | ".m4a" -> "audio/mp4"
  | ".webm" -> "audio/webm"
  | ".flac" -> "audio/flac"
  | _ -> "application/octet-stream"

let transcribe ~(config : Runtime_config.t) ?api_key ~audio_data ~filename
    ~content_type () =
  let open Lwt.Syntax in
  match config.stt with
  | None -> Lwt.fail_with "No STT config found"
  | Some stt_cfg -> (
      match List.assoc_opt stt_cfg.provider config.providers with
      | None ->
          Lwt.fail_with
            (Printf.sprintf "STT provider '%s' not found" stt_cfg.provider)
      | Some provider -> (
          let base_url =
            match provider.base_url with
            | Some url -> url
            | None -> "https://api.groq.com/openai/v1"
          in
          let uri = base_url ^ "/audio/transcriptions" in
          let resolved_api_key =
            match api_key with Some k when k <> "" -> k | _ -> provider.api_key
          in
          let headers = [ ("Authorization", "Bearer " ^ resolved_api_key) ] in
          let parts =
            [
              Http_client.File
                { name = "file"; filename; content_type; data = audio_data };
              Http_client.Field { name = "model"; value = stt_cfg.model };
            ]
            @
            match stt_cfg.language with
            | Some lang ->
                [ Http_client.Field { name = "language"; value = lang } ]
            | None -> []
          in
          Logs.info (fun m ->
              m "STT request to %s model=%s file=%s" uri stt_cfg.model filename);
          let* status, body = Http_client.post_multipart ~uri ~headers ~parts in
          if status < 200 || status >= 300 then
            Lwt.fail_with
              (Printf.sprintf "STT API error (HTTP %d): %s" status body)
          else
            let json =
              try Ok (Yojson.Safe.from_string body)
              with exn -> Error (Printexc.to_string exn)
            in
            match json with
            | Error msg -> Lwt.fail_with ("Failed to parse STT response: " ^ msg)
            | Ok json -> (
                try
                  let text =
                    Yojson.Safe.Util.(json |> member "text" |> to_string)
                  in
                  Lwt.return { text }
                with _ ->
                  Lwt.fail_with "Failed to extract text from STT response")))
