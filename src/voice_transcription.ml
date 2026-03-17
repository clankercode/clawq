let is_audio_mime mime =
  String.length mime >= 6 && String.sub mime 0 6 = "audio/"

let audio_extensions =
  [ ".ogg"; ".mp3"; ".wav"; ".m4a"; ".webm"; ".flac"; ".opus"; ".aac"; ".wma" ]

let is_audio_filename filename =
  let ext = Filename.extension filename |> String.lowercase_ascii in
  List.mem ext audio_extensions

(* Voice-first formats: .ogg, .opus, .webm are commonly used for voice
   messages, not music. The remaining audio formats are more likely music
   unless the filename suggests a voice recording. *)
let voice_format_extensions = [ ".ogg"; ".opus"; ".webm" ]

let contains_ci haystack needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nlen = String.length n in
  let hlen = String.length h in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if (not !found) && String.sub h i nlen = n then found := true
    done;
    !found

let is_likely_music ~filename ~mime_type:_ =
  let ext = Filename.extension filename |> String.lowercase_ascii in
  if List.mem ext voice_format_extensions then false
  else
    not
      (contains_ci filename "voice"
      || contains_ci filename "recording"
      || contains_ci filename "audio_message")

let max_audio_bytes = 25 * 1024 * 1024
let max_duration_seconds = 3600

type skip_reason =
  | TooLarge of int
  | TooLong of int
  | LikelyMusic
  | NoSttConfig

let skip_reason_to_string = function
  | TooLarge size ->
      Printf.sprintf "Audio too large (%d bytes, max %d)" size max_audio_bytes
  | TooLong duration ->
      Printf.sprintf "Audio too long (%d seconds, max %d)" duration
        max_duration_seconds
  | LikelyMusic -> "Audio file appears to be music, not a voice message"
  | NoSttConfig -> "No STT configuration found"

let validate ~(config : Runtime_config.t) ~filename ~mime_type ~size
    ~duration_seconds =
  if config.stt = None then Error NoSttConfig
  else if is_likely_music ~filename ~mime_type then Error LikelyMusic
  else
    match size with
    | Some s when s > max_audio_bytes -> Error (TooLarge s)
    | _ -> (
        match duration_seconds with
        | Some d when d > max_duration_seconds -> Error (TooLong d)
        | _ -> Ok ())

let transcribe_with_progress ~(config : Runtime_config.t)
    ~(notifier : Status_message.notifier) ~audio_data ~filename () =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* msg_id = notifier.send "Transcribing..." in
      let content_type = Stt.content_type_of_ext filename in
      let* result =
        Stt.transcribe ~config ~audio_data ~filename ~content_type ()
      in
      let* _new_id = notifier.edit msg_id "Transcribing... Done!" in
      Lwt.return ("[Voice]: " ^ result.text))
    (fun exn ->
      Logs.err (fun m ->
          m "Voice transcription failed: %s" (Printexc.to_string exn));
      Lwt.return "")
