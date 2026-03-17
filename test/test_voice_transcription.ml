let test_is_audio_mime () =
  Alcotest.(check bool)
    "audio/ogg is audio" true
    (Voice_transcription.is_audio_mime "audio/ogg");
  Alcotest.(check bool)
    "audio/mpeg is audio" true
    (Voice_transcription.is_audio_mime "audio/mpeg");
  Alcotest.(check bool)
    "audio/wav is audio" true
    (Voice_transcription.is_audio_mime "audio/wav");
  Alcotest.(check bool)
    "image/png is not audio" false
    (Voice_transcription.is_audio_mime "image/png");
  Alcotest.(check bool)
    "text/plain is not audio" false
    (Voice_transcription.is_audio_mime "text/plain");
  Alcotest.(check bool)
    "empty string" false
    (Voice_transcription.is_audio_mime "")

let test_is_audio_filename () =
  Alcotest.(check bool)
    "voice.ogg" true
    (Voice_transcription.is_audio_filename "voice.ogg");
  Alcotest.(check bool)
    "recording.mp3" true
    (Voice_transcription.is_audio_filename "recording.mp3");
  Alcotest.(check bool)
    "memo.m4a" true
    (Voice_transcription.is_audio_filename "memo.m4a");
  Alcotest.(check bool)
    "track.flac" true
    (Voice_transcription.is_audio_filename "track.flac");
  Alcotest.(check bool)
    "file.opus" true
    (Voice_transcription.is_audio_filename "file.opus");
  Alcotest.(check bool)
    "file.aac" true
    (Voice_transcription.is_audio_filename "file.aac");
  Alcotest.(check bool)
    "file.wma" true
    (Voice_transcription.is_audio_filename "file.wma");
  Alcotest.(check bool)
    "photo.png" false
    (Voice_transcription.is_audio_filename "photo.png");
  Alcotest.(check bool)
    "doc.pdf" false
    (Voice_transcription.is_audio_filename "doc.pdf")

let test_is_likely_music () =
  (* Voice-first formats never flagged as music *)
  Alcotest.(check bool)
    "voice.ogg -> not music" false
    (Voice_transcription.is_likely_music ~filename:"voice.ogg" ~mime_type:None);
  Alcotest.(check bool)
    "msg.opus -> not music" false
    (Voice_transcription.is_likely_music ~filename:"msg.opus" ~mime_type:None);
  Alcotest.(check bool)
    "clip.webm -> not music" false
    (Voice_transcription.is_likely_music ~filename:"clip.webm" ~mime_type:None);
  (* Music-format files are flagged as music *)
  Alcotest.(check bool)
    "track.mp3 -> music" true
    (Voice_transcription.is_likely_music ~filename:"track.mp3" ~mime_type:None);
  Alcotest.(check bool)
    "song.m4a -> music" true
    (Voice_transcription.is_likely_music ~filename:"song.m4a" ~mime_type:None);
  Alcotest.(check bool)
    "album.flac -> music" true
    (Voice_transcription.is_likely_music ~filename:"album.flac" ~mime_type:None);
  Alcotest.(check bool)
    "file.aac -> music" true
    (Voice_transcription.is_likely_music ~filename:"file.aac" ~mime_type:None);
  Alcotest.(check bool)
    "file.wma -> music" true
    (Voice_transcription.is_likely_music ~filename:"file.wma" ~mime_type:None);
  (* Voice-indicating filenames override music heuristic *)
  Alcotest.(check bool)
    "voice_recording.mp3 -> not music" false
    (Voice_transcription.is_likely_music ~filename:"voice_recording.mp3"
       ~mime_type:None);
  Alcotest.(check bool)
    "Voice_Note.m4a -> not music" false
    (Voice_transcription.is_likely_music ~filename:"Voice_Note.m4a"
       ~mime_type:None);
  Alcotest.(check bool)
    "my_recording.aac -> not music" false
    (Voice_transcription.is_likely_music ~filename:"my_recording.aac"
       ~mime_type:None);
  Alcotest.(check bool)
    "audio_message_001.mp3 -> not music" false
    (Voice_transcription.is_likely_music ~filename:"audio_message_001.mp3"
       ~mime_type:None)

let make_config_with_stt () =
  {
    Runtime_config.default with
    stt =
      Some { provider = "groq"; model = "whisper-large-v3"; language = None };
  }

let make_config_without_stt () = Runtime_config.default

let test_validate_no_stt () =
  let config = make_config_without_stt () in
  let result =
    Voice_transcription.validate ~config ~filename:"voice.ogg"
      ~mime_type:(Some "audio/ogg") ~size:None ~duration_seconds:None
  in
  Alcotest.(check bool) "no stt -> error" true (Result.is_error result);
  match result with
  | Error Voice_transcription.NoSttConfig -> ()
  | _ -> Alcotest.fail "expected NoSttConfig"

let test_validate_too_large () =
  let config = make_config_with_stt () in
  let big_size = Voice_transcription.max_audio_bytes + 1 in
  let result =
    Voice_transcription.validate ~config ~filename:"voice.ogg"
      ~mime_type:(Some "audio/ogg") ~size:(Some big_size) ~duration_seconds:None
  in
  Alcotest.(check bool) "too large -> error" true (Result.is_error result);
  match result with
  | Error (Voice_transcription.TooLarge _) -> ()
  | _ -> Alcotest.fail "expected TooLarge"

let test_validate_too_long () =
  let config = make_config_with_stt () in
  let long_duration = Voice_transcription.max_duration_seconds + 1 in
  let result =
    Voice_transcription.validate ~config ~filename:"voice.ogg"
      ~mime_type:(Some "audio/ogg") ~size:None
      ~duration_seconds:(Some long_duration)
  in
  Alcotest.(check bool) "too long -> error" true (Result.is_error result);
  match result with
  | Error (Voice_transcription.TooLong _) -> ()
  | _ -> Alcotest.fail "expected TooLong"

let test_validate_music () =
  let config = make_config_with_stt () in
  let result =
    Voice_transcription.validate ~config ~filename:"track.mp3"
      ~mime_type:(Some "audio/mpeg") ~size:None ~duration_seconds:None
  in
  Alcotest.(check bool) "music -> error" true (Result.is_error result);
  match result with
  | Error Voice_transcription.LikelyMusic -> ()
  | _ -> Alcotest.fail "expected LikelyMusic"

let test_validate_ok () =
  let config = make_config_with_stt () in
  let result =
    Voice_transcription.validate ~config ~filename:"voice.ogg"
      ~mime_type:(Some "audio/ogg") ~size:(Some 1000)
      ~duration_seconds:(Some 60)
  in
  Alcotest.(check bool) "valid -> ok" true (Result.is_ok result)

let test_validate_voice_named_mp3 () =
  let config = make_config_with_stt () in
  let result =
    Voice_transcription.validate ~config ~filename:"voice_recording.mp3"
      ~mime_type:(Some "audio/mpeg") ~size:(Some 1000) ~duration_seconds:None
  in
  Alcotest.(check bool) "voice-named mp3 -> ok" true (Result.is_ok result)

let test_skip_reason_strings () =
  let s =
    Voice_transcription.skip_reason_to_string Voice_transcription.NoSttConfig
  in
  Alcotest.(check bool) "NoSttConfig has content" true (String.length s > 0);
  let s =
    Voice_transcription.skip_reason_to_string Voice_transcription.LikelyMusic
  in
  Alcotest.(check bool) "LikelyMusic has content" true (String.length s > 0);
  let s =
    Voice_transcription.skip_reason_to_string (Voice_transcription.TooLarge 100)
  in
  Alcotest.(check bool) "TooLarge has content" true (String.length s > 0);
  let s =
    Voice_transcription.skip_reason_to_string (Voice_transcription.TooLong 100)
  in
  Alcotest.(check bool) "TooLong has content" true (String.length s > 0)

let test_max_constants () =
  Alcotest.(check int)
    "max_audio_bytes is 25MB"
    (25 * 1024 * 1024)
    Voice_transcription.max_audio_bytes;
  Alcotest.(check int)
    "max_duration is 3600s" 3600 Voice_transcription.max_duration_seconds

let suite =
  [
    Alcotest.test_case "is_audio_mime" `Quick test_is_audio_mime;
    Alcotest.test_case "is_audio_filename" `Quick test_is_audio_filename;
    Alcotest.test_case "is_likely_music" `Quick test_is_likely_music;
    Alcotest.test_case "validate no stt" `Quick test_validate_no_stt;
    Alcotest.test_case "validate too large" `Quick test_validate_too_large;
    Alcotest.test_case "validate too long" `Quick test_validate_too_long;
    Alcotest.test_case "validate music" `Quick test_validate_music;
    Alcotest.test_case "validate ok" `Quick test_validate_ok;
    Alcotest.test_case "validate voice-named mp3" `Quick
      test_validate_voice_named_mp3;
    Alcotest.test_case "skip reason strings" `Quick test_skip_reason_strings;
    Alcotest.test_case "max constants" `Quick test_max_constants;
  ]
