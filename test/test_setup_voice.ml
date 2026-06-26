(* test_setup_voice.ml — Unit tests for Setup_voice pure functions *)

let validate_voice_nova () =
  Alcotest.(check (result string string))
    "nova ok" (Ok "nova")
    (Setup_voice.validate_voice "nova")

let validate_voice_all () =
  let voices = [ "alloy"; "echo"; "fable"; "onyx"; "nova"; "shimmer" ] in
  List.iter
    (fun v ->
      match Setup_voice.validate_voice v with
      | Ok r -> Alcotest.(check string) ("voice " ^ v) v r
      | Error e -> Alcotest.failf "expected Ok for '%s', got Error: %s" v e)
    voices

let validate_voice_invalid () =
  match Setup_voice.validate_voice "invalid_voice" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid voice"

let validate_voice_empty () =
  match Setup_voice.validate_voice "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty voice"

let validate_voice_uppercase () =
  match Setup_voice.validate_voice "Nova" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for uppercase voice"

let validate_speed_normal () =
  Alcotest.(check (result (float 0.01) string))
    "1.0 ok" (Ok 1.0)
    (Setup_voice.validate_speed "1.0")

let validate_speed_min () =
  Alcotest.(check (result (float 0.001) string))
    "0.25 ok" (Ok 0.25)
    (Setup_voice.validate_speed "0.25")

let validate_speed_max () =
  Alcotest.(check (result (float 0.01) string))
    "4.0 ok" (Ok 4.0)
    (Setup_voice.validate_speed "4.0")

let validate_speed_too_low () =
  match Setup_voice.validate_speed "0.1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for speed 0.1"

let validate_speed_too_high () =
  match Setup_voice.validate_speed "4.1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for speed 4.1"

let validate_speed_non_float () =
  match Setup_voice.validate_speed "fast" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-float speed"

let validate_speed_zero () =
  match Setup_voice.validate_speed "0.0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for speed 0.0"

let build_json_tts_only () =
  let json =
    Setup_voice.build_voice_json ~stt_enabled:false ~tts_enabled:true
      ~stt_provider:"openai" ~tts_provider:"openai" ~tts_model:"tts-1"
      ~tts_voice:"nova" ~tts_speed:1.0 ~audio_dir:"/tmp/audio"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.voice with
  | Some v ->
      Alcotest.(check bool) "stt_enabled" false v.stt_enabled;
      Alcotest.(check bool) "tts_enabled" true v.tts_enabled;
      Alcotest.(check string) "stt_provider" "openai" v.stt_provider;
      Alcotest.(check string) "tts_provider" "openai" v.tts_provider;
      Alcotest.(check string) "tts_model" "tts-1" v.tts_model;
      Alcotest.(check string) "tts_voice" "nova" v.tts_voice;
      Alcotest.(check (float 0.001)) "tts_speed" 1.0 v.tts_speed;
      Alcotest.(check string) "audio_dir" "/tmp/audio" v.audio_dir
  | None -> Alcotest.fail "expected voice config"

let build_json_both_enabled () =
  let json =
    Setup_voice.build_voice_json ~stt_enabled:true ~tts_enabled:true
      ~stt_provider:"whisper-local" ~tts_provider:"openai" ~tts_model:"tts-1-hd"
      ~tts_voice:"alloy" ~tts_speed:1.5 ~audio_dir:"/tmp/voice"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.voice with
  | Some v ->
      Alcotest.(check bool) "stt_enabled" true v.stt_enabled;
      Alcotest.(check bool) "tts_enabled" true v.tts_enabled;
      Alcotest.(check string) "stt_provider" "whisper-local" v.stt_provider;
      Alcotest.(check string) "tts_model" "tts-1-hd" v.tts_model;
      Alcotest.(check string) "tts_voice" "alloy" v.tts_voice;
      Alcotest.(check (float 0.001)) "tts_speed" 1.5 v.tts_speed
  | None -> Alcotest.fail "expected voice config"

let build_json_both_disabled_returns_none () =
  (* config_loader returns None if neither stt_enabled nor tts_enabled *)
  let json =
    Setup_voice.build_voice_json ~stt_enabled:false ~tts_enabled:false
      ~stt_provider:"openai" ~tts_provider:"openai" ~tts_model:"tts-1"
      ~tts_voice:"nova" ~tts_speed:1.0 ~audio_dir:"/tmp/audio"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(
    check (option (Alcotest.testable (fun _ _ -> ()) (fun _ _ -> true))))
    "voice is None when both disabled" None config.voice

let suite =
  [
    Alcotest.test_case "validate_voice nova" `Quick validate_voice_nova;
    Alcotest.test_case "validate_voice all" `Quick validate_voice_all;
    Alcotest.test_case "validate_voice invalid" `Quick validate_voice_invalid;
    Alcotest.test_case "validate_voice empty" `Quick validate_voice_empty;
    Alcotest.test_case "validate_voice uppercase" `Quick
      validate_voice_uppercase;
    Alcotest.test_case "validate_speed 1.0" `Quick validate_speed_normal;
    Alcotest.test_case "validate_speed min 0.25" `Quick validate_speed_min;
    Alcotest.test_case "validate_speed max 4.0" `Quick validate_speed_max;
    Alcotest.test_case "validate_speed too low" `Quick validate_speed_too_low;
    Alcotest.test_case "validate_speed too high" `Quick validate_speed_too_high;
    Alcotest.test_case "validate_speed non-float" `Quick
      validate_speed_non_float;
    Alcotest.test_case "validate_speed zero" `Quick validate_speed_zero;
    Alcotest.test_case "build_json tts only" `Quick build_json_tts_only;
    Alcotest.test_case "build_json both enabled" `Quick build_json_both_enabled;
    Alcotest.test_case "build_json both disabled returns none" `Quick
      build_json_both_disabled_returns_none;
  ]
