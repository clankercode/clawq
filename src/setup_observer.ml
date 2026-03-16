(* setup_observer.ml — Interactive setup wizard for observer configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_model s =
  if String.trim s = "" then Error "Model must not be empty."
  else match Pmodel.parse s with Ok _ -> Ok s | Error msg -> Error msg

let validate_positive_int s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Value must be a positive integer."
  | None -> Error "Value must be a valid integer."

let build_observer_json ~enabled ~model ~check_every_n_messages ~round1_window
    ~round2_window ~thinking_token_threshold ~consecutive_errors_threshold
    ~repeat_call_threshold =
  `Assoc
    [
      ( "observer",
        `Assoc
          [
            ("enabled", `Bool enabled);
            ("model", `String model);
            ("check_every_n_messages", `Int check_every_n_messages);
            ("round1_window", `Int round1_window);
            ("round2_window", `Int round2_window);
            ("thinking_token_threshold", `Int thinking_token_threshold);
            ("consecutive_errors_threshold", `Int consecutive_errors_threshold);
            ("repeat_call_threshold", `Int repeat_call_threshold);
          ] );
    ]

let post_setup_instructions =
  {|
  Observer setup:

    The observer monitors agent conversations for quality and safety issues.
    It periodically reviews recent messages and can detect stuck agents,
    excessive tool call loops, runaway thinking, and consecutive errors.

    enabled:                       Whether the observer is active.
    model:                         The model used for observation checks.
                                   Use canonical provider:model format
                                   (e.g. groq:openai/gpt-oss-120b).
    check_every_n_messages:        How often (in messages) to run a check.
    round1_window:                 Number of recent messages in a quick check.
    round2_window:                 Number of messages in a deeper check.
    thinking_token_threshold:      Token count triggering a thinking warning.
    consecutive_errors_threshold:  Consecutive errors before intervention.
    repeat_call_threshold:         Repeated identical tool calls before alert.

  After saving:

    - Start the daemon: clawq daemon start

  Full documentation: https://clawq.org/observer/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try (Config_loader.load ()).observer
  with _ -> Runtime_config.default_observer_config

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle observer enabled"
      ~description:"Whether the observer monitors agent conversations."
      ~default:existing.enabled ()
  in
  let model =
    Setup_tui.make_field ~key:"m" ~label:"Model"
      ~menu_label:"Set observer model"
      ~description:
        "Model for observation checks. Use canonical provider:model format."
      ~validate:validate_model
      ~default:(Pmodel.to_string existing.model)
      ()
  in
  let check_every_n_messages =
    Setup_tui.make_int_field ~key:"c" ~label:"Check every N messages"
      ~menu_label:"Set check frequency (messages)"
      ~description:"Number of messages between observer checks."
      ~validate:validate_positive_int ~default:existing.check_every_n_messages
      ()
  in
  let round1_window =
    Setup_tui.make_int_field ~key:"1" ~label:"Round 1 window"
      ~menu_label:"Set round 1 window"
      ~description:"Number of recent messages used in the quick check pass."
      ~validate:validate_positive_int ~default:existing.round1_window ()
  in
  let round2_window =
    Setup_tui.make_int_field ~key:"2" ~label:"Round 2 window"
      ~menu_label:"Set round 2 window"
      ~description:"Number of messages used in the deeper check pass."
      ~validate:validate_positive_int ~default:existing.round2_window ()
  in
  let thinking_token_threshold =
    Setup_tui.make_int_field ~key:"k" ~label:"Thinking token threshold"
      ~menu_label:"Set thinking token threshold"
      ~description:"Token count that triggers a thinking-runaway warning."
      ~validate:validate_positive_int ~default:existing.thinking_token_threshold
      ()
  in
  let consecutive_errors_threshold =
    Setup_tui.make_int_field ~key:"r" ~label:"Consecutive errors threshold"
      ~menu_label:"Set consecutive errors threshold"
      ~description:
        "Number of consecutive errors before the observer intervenes."
      ~validate:validate_positive_int
      ~default:existing.consecutive_errors_threshold ()
  in
  let repeat_call_threshold =
    Setup_tui.make_int_field ~key:"p" ~label:"Repeat call threshold"
      ~menu_label:"Set repeat call threshold"
      ~description:
        "Number of identical repeated tool calls before an alert is raised."
      ~validate:validate_positive_int ~default:existing.repeat_call_threshold ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Observer Configuration ";
      docs_url = "https://clawq.org/observer/";
      fields =
        [
          enabled;
          model;
          check_every_n_messages;
          round1_window;
          round2_window;
          thinking_token_threshold;
          consecutive_errors_threshold;
          repeat_call_threshold;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_observer_json
            ~enabled:(Setup_tui.get_bool enabled)
            ~model:(Setup_tui.get_str model)
            ~check_every_n_messages:(Setup_tui.get_int check_every_n_messages)
            ~round1_window:(Setup_tui.get_int round1_window)
            ~round2_window:(Setup_tui.get_int round2_window)
            ~thinking_token_threshold:
              (Setup_tui.get_int thinking_token_threshold)
            ~consecutive_errors_threshold:
              (Setup_tui.get_int consecutive_errors_threshold)
            ~repeat_call_threshold:(Setup_tui.get_int repeat_call_threshold));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
