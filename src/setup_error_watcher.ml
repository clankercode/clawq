(* setup_error_watcher.ml — Interactive setup wizard for error_watcher config *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_scan_interval s =
  match float_of_string_opt s with
  | Some v when v > 0.0 -> Ok s
  | Some _ -> Error "Scan interval must be greater than 0."
  | None -> Error "Scan interval must be a valid number."

let validate_cooldown s =
  match float_of_string_opt s with
  | Some v when v > 0.0 -> Ok s
  | Some _ -> Error "Cooldown must be greater than 0."
  | None -> Error "Cooldown must be a valid number."

let validate_max_errors s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Max errors per batch must be a positive integer."
  | None -> Error "Max errors per batch must be a valid integer."

let validate_commit_tag s =
  if String.trim s = "" then Error "Commit tag must not be empty." else Ok s

let build_error_watcher_json ~enabled ~scan_interval_s ~primary_models
    ~fallback_models ~cooldown_s ~max_errors_per_batch ~ignore_patterns
    ~auto_fix_enabled ~commit_tag =
  `Assoc
    [
      ( "error_watcher",
        `Assoc
          [
            ("enabled", `Bool enabled);
            ("scan_interval_s", `Float scan_interval_s);
            ( "primary_models",
              `List (List.map (fun s -> `String s) primary_models) );
            ( "fallback_models",
              `List (List.map (fun s -> `String s) fallback_models) );
            ("cooldown_s", `Float cooldown_s);
            ("max_errors_per_batch", `Int max_errors_per_batch);
            ( "ignore_patterns",
              `List (List.map (fun s -> `String s) ignore_patterns) );
            ("auto_fix_enabled", `Bool auto_fix_enabled);
            ("commit_tag", `String commit_tag);
          ] );
    ]

let post_setup_instructions =
  {|
  Error Watcher setup:

    The error watcher scans logs and outputs for errors, then uses AI models
    to diagnose and optionally fix them automatically.

    enabled:              Whether the error watcher runs at all.
    scan_interval_s:      How often (in seconds) to scan for new errors.
    primary_models:       Models tried first for diagnosis/fixing.
    fallback_models:      Models used if primary models are unavailable.
    cooldown_s:           Minimum seconds between error-watcher activations.
    max_errors_per_batch: Maximum errors to process in a single scan cycle.
    ignore_patterns:      Comma-separated patterns to skip (e.g. "WARN,TODO").
    auto_fix_enabled:     Whether to automatically apply AI-suggested fixes.
    commit_tag:           Tag prepended to auto-fix commit messages.

  After saving:

    - Start the daemon: clawq daemon start
    - Introduce a test error to verify detection.

  Full documentation: https://clawq.org/error-watcher/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try (Config_loader.load ()).error_watcher
  with _ -> Runtime_config.default_error_watcher_config

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle error watcher enabled"
      ~description:"Whether the error watcher is active."
      ~default:existing.enabled ()
  in
  let scan_interval_s =
    Setup_tui.make_float_field ~key:"i" ~label:"Scan interval (s)"
      ~menu_label:"Set scan interval (seconds)"
      ~description:"How often to scan for new errors. Must be > 0."
      ~validate:validate_scan_interval ~default:existing.scan_interval_s ()
  in
  let primary_models =
    Setup_tui.make_list_field ~key:"p" ~label:"Primary models"
      ~menu_label:"Set primary models"
      ~description:
        "Comma-separated list of models to try first (e.g. \
         anthropic:claude-opus-4-6)."
      ~default:existing.primary_models ()
  in
  let fallback_models =
    Setup_tui.make_list_field ~key:"f" ~label:"Fallback models"
      ~menu_label:"Set fallback models"
      ~description:
        "Comma-separated list of fallback models used when primary models are \
         unavailable."
      ~default:existing.fallback_models ()
  in
  let cooldown_s =
    Setup_tui.make_float_field ~key:"d" ~label:"Cooldown (s)"
      ~menu_label:"Set cooldown (seconds)"
      ~description:
        "Minimum seconds between error-watcher activations. Must be > 0."
      ~validate:validate_cooldown ~default:existing.cooldown_s ()
  in
  let max_errors_per_batch =
    Setup_tui.make_int_field ~key:"m" ~label:"Max errors per batch"
      ~menu_label:"Set max errors per batch"
      ~description:"Maximum errors to process in one scan cycle."
      ~validate:validate_max_errors ~default:existing.max_errors_per_batch ()
  in
  let ignore_patterns =
    Setup_tui.make_list_field ~key:"g" ~label:"Ignore patterns"
      ~menu_label:"Set ignore patterns"
      ~description:
        "Comma-separated patterns to ignore (e.g. WARN,TODO). Leave empty to \
         process all errors."
      ~default:existing.ignore_patterns ()
  in
  let auto_fix_enabled =
    Setup_tui.make_bool_field ~key:"a" ~label:"Auto-fix enabled"
      ~menu_label:"Toggle auto-fix"
      ~description:"Whether to automatically apply AI-suggested fixes."
      ~default:existing.auto_fix_enabled ()
  in
  let commit_tag =
    Setup_tui.make_field ~key:"t" ~label:"Commit tag"
      ~menu_label:"Set commit tag"
      ~description:"Tag prepended to auto-fix commit messages."
      ~validate:validate_commit_tag ~default:existing.commit_tag ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Error Watcher Configuration ";
      docs_url = "https://clawq.org/error-watcher/";
      fields =
        [
          enabled;
          scan_interval_s;
          primary_models;
          fallback_models;
          cooldown_s;
          max_errors_per_batch;
          ignore_patterns;
          auto_fix_enabled;
          commit_tag;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_error_watcher_json
            ~enabled:(Setup_tui.get_bool enabled)
            ~scan_interval_s:(Setup_tui.get_float scan_interval_s)
            ~primary_models:(Setup_tui.get_str_list primary_models)
            ~fallback_models:(Setup_tui.get_str_list fallback_models)
            ~cooldown_s:(Setup_tui.get_float cooldown_s)
            ~max_errors_per_batch:(Setup_tui.get_int max_errors_per_batch)
            ~ignore_patterns:(Setup_tui.get_str_list ignore_patterns)
            ~auto_fix_enabled:(Setup_tui.get_bool auto_fix_enabled)
            ~commit_tag:(Setup_tui.get_str commit_tag));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
