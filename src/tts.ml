type voice = Alloy | Echo | Fable | Onyx | Nova | Shimmer

let voice_to_string = function
  | Alloy -> "alloy"
  | Echo -> "echo"
  | Fable -> "fable"
  | Onyx -> "onyx"
  | Nova -> "nova"
  | Shimmer -> "shimmer"

let voice_of_string = function
  | "echo" -> Echo
  | "fable" -> Fable
  | "onyx" -> Onyx
  | "nova" -> Nova
  | "shimmer" -> Shimmer
  | _ -> Alloy

let synthesize ~(config : Runtime_config.t)
    ~(voice_config : Runtime_config.voice_config) ~text () =
  let open Lwt.Syntax in
  match List.assoc_opt voice_config.tts_provider config.providers with
  | None ->
      Lwt.fail_with
        (Printf.sprintf "TTS provider '%s' not found" voice_config.tts_provider)
  | Some provider ->
      let base_url =
        match provider.base_url with
        | Some url -> url
        | None -> "https://api.openai.com/v1"
      in
      let uri = base_url ^ "/audio/speech" in
      let voice_str =
        voice_of_string voice_config.tts_voice |> voice_to_string
      in
      let body =
        `Assoc
          [
            ("model", `String voice_config.tts_model);
            ("input", `String text);
            ("voice", `String voice_str);
          ]
        |> Yojson.Safe.to_string
      in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m -> m "TTS request to %s voice=%s" uri voice_str);
      let* status, response_body = Http_client.post_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then
        Lwt.fail_with
          (Printf.sprintf "TTS API error (HTTP %d): %s" status response_body)
      else begin
        (try
           if not (Sys.file_exists voice_config.audio_dir) then
             Sys.mkdir voice_config.audio_dir 0o755
         with _ -> ());
        let filename =
          Printf.sprintf "tts_%d_%s.mp3"
            (int_of_float (Unix.gettimeofday () *. 1000.0))
            (voice_to_string (voice_of_string voice_config.tts_voice))
        in
        let path = Filename.concat voice_config.audio_dir filename in
        let oc = open_out_bin path in
        output_string oc response_body;
        close_out oc;
        Logs.info (fun m -> m "TTS output saved to %s" path);
        Lwt.return path
      end

let transcribe_if_voice ~(config : Runtime_config.t)
    ~(voice_config : Runtime_config.voice_config option) ~message () =
  let open Lwt.Syntax in
  let prefix = "[VOICE:" in
  let plen = String.length prefix in
  if String.length message > plen && String.sub message 0 plen = prefix then
    match voice_config with
    | None -> Lwt.return message
    | Some _vc -> (
        let close_bracket =
          try Some (String.index message ']') with Not_found -> None
        in
        match close_bracket with
        | None -> Lwt.return message
        | Some idx ->
            let audio_path = String.sub message plen (idx - plen) in
            let remaining =
              if idx + 1 < String.length message then
                String.sub message (idx + 1) (String.length message - idx - 1)
              else ""
            in
            Lwt.catch
              (fun () ->
                let ic = open_in_bin audio_path in
                let len = in_channel_length ic in
                let data = really_input_string ic len in
                close_in ic;
                let content_type = Stt.content_type_of_ext audio_path in
                let filename = Filename.basename audio_path in
                let* result =
                  Stt.transcribe ~config ~audio_data:data ~filename
                    ~content_type ()
                in
                let text = result.text in
                let final =
                  if remaining = "" then text
                  else text ^ " " ^ String.trim remaining
                in
                Lwt.return final)
              (fun exn ->
                Logs.warn (fun m ->
                    m "Voice transcription failed: %s" (Printexc.to_string exn));
                Lwt.return message))
  else Lwt.return message

let maybe_synthesize_reply ~(config : Runtime_config.t)
    ~(voice_config : Runtime_config.voice_config option) ~reply () =
  let open Lwt.Syntax in
  match voice_config with
  | Some vc when vc.tts_enabled ->
      Lwt.catch
        (fun () ->
          let* path = synthesize ~config ~voice_config:vc ~text:reply () in
          Lwt.return (Some path))
        (fun exn ->
          Logs.warn (fun m ->
              m "TTS synthesis failed: %s" (Printexc.to_string exn));
          Lwt.return_none)
  | _ -> Lwt.return_none
