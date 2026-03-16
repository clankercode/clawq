(* setup_voice.ml — Setup wizard for voice (TTS/STT) configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let valid_voices = [ "alloy"; "echo"; "fable"; "onyx"; "nova"; "shimmer" ]

let validate_voice s =
  let trimmed = String.trim s in
  if List.mem trimmed valid_voices then Ok trimmed
  else
    Error
      (Printf.sprintf "Voice must be one of: %s"
         (String.concat ", " valid_voices))

let validate_speed s =
  match float_of_string_opt (String.trim s) with
  | None -> Error "Speed must be a number."
  | Some f when f < 0.25 -> Error "Speed must be at least 0.25."
  | Some f when f > 4.0 -> Error "Speed must be at most 4.0."
  | Some f -> Ok f

(* ── JSON builder ────────────────────────────────────────────────── *)

let build_voice_json ~stt_enabled ~tts_enabled ~tts_provider ~tts_model
    ~tts_voice ~tts_speed ~audio_dir =
  let fields =
    [
      ("stt_enabled", `Bool stt_enabled);
      ("tts_enabled", `Bool tts_enabled);
      ("stt_provider", `String "openai");
      ("tts_provider", `String tts_provider);
      ("tts_model", `String tts_model);
      ("tts_voice", `String tts_voice);
      ("tts_speed", `Float tts_speed);
      ("audio_dir", `String audio_dir);
    ]
  in
  `Assoc [ ("voice", `Assoc fields) ]

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.voice
  with _ -> None

(* ── Run wizard ──────────────────────────────────────────────────── *)

let run () =
  let stt_enabled_field =
    Setup_tui.make_bool_field ~key:"r" ~label:"STT Enabled"
      ~menu_label:"Toggle speech-to-text"
      ~description:
        "Enable speech-to-text transcription. Uses OpenAI Whisper API via the \
         configured openai-codex provider. Requires an API key."
      ()
  in
  let tts_enabled_field =
    Setup_tui.make_bool_field ~key:"t" ~label:"TTS Enabled"
      ~menu_label:"Toggle text-to-speech"
      ~description:
        "Enable text-to-speech synthesis. Converts agent responses to audio \
         files saved in the configured audio directory."
      ()
  in
  let tts_provider_field =
    Setup_tui.make_field ~key:"P" ~label:"TTS Provider"
      ~menu_label:"Set TTS provider"
      ~description:
        "TTS provider name (must match a configured provider). Default: \
         'openai' (uses openai-codex provider's TTS endpoint)."
      ~default:"openai" ()
  in
  let tts_model_field =
    Setup_tui.make_field ~key:"m" ~label:"TTS Model" ~menu_label:"Set TTS model"
      ~description:
        "TTS model: 'tts-1' (faster, lower quality) or 'tts-1-hd' (slower, \
         higher quality). Default: tts-1."
      ~default:"tts-1" ()
  in
  let tts_voice_field =
    Setup_tui.make_choice_field ~key:"v" ~label:"TTS Voice"
      ~menu_label:"Set TTS voice" ~choices:valid_voices
      ~description:
        "TTS voice character. alloy=neutral, echo=male, fable=british, \
         onyx=deep, nova=female friendly (default), shimmer=female soft."
      ~validate:validate_voice ~default:"nova" ()
  in
  let tts_speed_field =
    Setup_tui.make_float_field ~key:"x" ~label:"TTS Speed"
      ~menu_label:"Set TTS speed"
      ~description:
        "TTS speech speed multiplier (0.25 = very slow, 1.0 = normal, 4.0 = \
         very fast). Default: 1.0."
      ~validate:(fun s ->
        match validate_speed s with
        | Ok f -> Ok (string_of_float f)
        | Error e -> Error e)
      ~default:1.0 ()
  in
  let audio_dir_field =
    Setup_tui.make_field ~key:"d" ~label:"Audio Dir"
      ~menu_label:"Set audio output directory"
      ~description:
        "Directory where TTS audio files are saved. Defaults to \
         ~/.clawq/audio/. Will be created if it does not exist."
      ~default:(Dot_dir.sub "audio") ()
  in
  (* Load existing values *)
  (match load_existing () with
  | Some v ->
      stt_enabled_field.value := string_of_bool v.stt_enabled;
      tts_enabled_field.value := string_of_bool v.tts_enabled;
      tts_provider_field.value := v.tts_provider;
      tts_model_field.value := v.tts_model;
      tts_voice_field.value := v.tts_voice;
      audio_dir_field.value := v.audio_dir
  | None -> ());
  let spec : Setup_tui.wizard_spec =
    {
      title = " Voice Configuration ";
      docs_url = "https://clawq.org/features/#voice";
      fields =
        [
          stt_enabled_field;
          tts_enabled_field;
          tts_provider_field;
          tts_model_field;
          tts_voice_field;
          tts_speed_field;
          audio_dir_field;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          let stt_enabled = Setup_tui.get_bool stt_enabled_field in
          let tts_enabled = Setup_tui.get_bool tts_enabled_field in
          let tts_provider = Setup_tui.get_str tts_provider_field in
          let tts_model = Setup_tui.get_str tts_model_field in
          let tts_voice = Setup_tui.get_str tts_voice_field in
          let tts_speed = Setup_tui.get_float tts_speed_field in
          let audio_dir = Setup_tui.get_str audio_dir_field in
          build_voice_json ~stt_enabled ~tts_enabled ~tts_provider ~tts_model
            ~tts_voice ~tts_speed ~audio_dir);
      pre_save_check =
        (fun () ->
          let tts_enabled = Setup_tui.get_bool tts_enabled_field in
          let tts_provider = Setup_tui.get_str tts_provider_field in
          if tts_enabled && tts_provider = "" then
            Error "TTS provider cannot be empty when TTS is enabled."
          else Ok ());
      post_instructions =
        (fun () ->
          {|
  Voice Setup Instructions
  ========================

  Speech-to-Text (STT):
    - Transcribes audio files to text
    - Uses OpenAI Whisper API (requires openai-codex provider configured)
    - Provide audio files via the transcribe tool

  Text-to-Speech (TTS):
    - Converts agent responses to audio
    - Requires an OpenAI-compatible TTS API
    - Audio files are saved to the configured audio directory

  Voice choices:
    alloy   - neutral, balanced
    echo    - male, expressive
    fable   - british, narrative
    onyx    - deep, authoritative
    nova    - female, friendly (default)
    shimmer - female, soft

  Full documentation: https://clawq.org/features/#voice
|});
    }
  in
  Setup_tui.run_wizard spec
