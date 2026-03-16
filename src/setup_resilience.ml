(* setup_resilience.ml — Interactive setup wizard for resilience configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_timeout s =
  match float_of_string_opt s with
  | Some v when v > 0.0 -> Ok s
  | Some _ -> Error "Timeout must be greater than 0."
  | None -> Error "Timeout must be a valid number."

let validate_retries s =
  match int_of_string_opt s with
  | Some v when v >= 0 -> Ok s
  | Some _ -> Error "Max retries must be 0 or greater."
  | None -> Error "Max retries must be a valid integer."

let validate_delay s =
  match float_of_string_opt s with
  | Some v when v > 0.0 -> Ok s
  | Some _ -> Error "Base delay must be greater than 0."
  | None -> Error "Base delay must be a valid number."

let build_resilience_json ~timeout_s ~max_retries ~base_delay_s
    ~fallback_provider =
  let fp_val =
    if fallback_provider = "" then `Null else `String fallback_provider
  in
  `Assoc
    [
      ( "resilience",
        `Assoc
          [
            ("timeout_s", `Float timeout_s);
            ("max_retries", `Int max_retries);
            ("base_delay_s", `Float base_delay_s);
            ("fallback_provider", fp_val);
          ] );
    ]

let post_setup_instructions =
  {|
  Resilience configuration setup:

    1. timeout_s: Seconds before an LLM API call is considered timed out.
       Keep this higher than any connector long-poll timeout to avoid false
       timeouts on normal long-poll waits. Default: 120.
    2. max_retries: Number of retry attempts after a failed API call. Default: 2.
    3. base_delay_s: Base delay in seconds between retries (exponential backoff).
       Default: 1.0.
    4. fallback_provider: Provider name to fall back to if the primary fails.
       Leave blank to disable fallback. e.g. "anthropic" or "openai-codex".

  After saving:

    - Restart the daemon: clawq daemon restart
    - Verify: clawq status

  Full documentation: https://clawq.org/resilience/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    Some cfg.resilience
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let default = Runtime_config.default.resilience in
  let get_r f = match existing with Some c -> f c | None -> f default in
  let timeout_s =
    Setup_tui.make_float_field ~key:"t" ~label:"Timeout (s)"
      ~menu_label:"Set request timeout (seconds)"
      ~description:"Seconds before an LLM API call times out. Must be > 0."
      ~validate:validate_timeout
      ~default:(get_r (fun c -> c.Runtime_config.timeout_s))
      ()
  in
  let max_retries =
    Setup_tui.make_int_field ~key:"r" ~label:"Max retries"
      ~menu_label:"Set max retry attempts"
      ~description:
        "Number of retry attempts after a failed API call. 0 disables retries."
      ~validate:validate_retries
      ~default:(get_r (fun c -> c.Runtime_config.max_retries))
      ()
  in
  let base_delay_s =
    Setup_tui.make_float_field ~key:"d" ~label:"Base delay (s)"
      ~menu_label:"Set base retry delay (seconds)"
      ~description:
        "Base delay between retries. Actual delay uses exponential backoff."
      ~validate:validate_delay
      ~default:(get_r (fun c -> c.Runtime_config.base_delay_s))
      ()
  in
  let fallback_provider =
    Setup_tui.make_field ~key:"f" ~label:"Fallback provider"
      ~menu_label:"Set fallback provider (optional)"
      ~description:
        "Provider to fall back to if primary fails. Leave blank to disable."
      ~default:
        (match get_r (fun c -> c.Runtime_config.fallback_provider) with
        | Some s -> s
        | None -> "")
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Resilience Configuration ";
      docs_url = "https://clawq.org/resilience/";
      fields = [ timeout_s; max_retries; base_delay_s; fallback_provider ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_resilience_json
            ~timeout_s:(Setup_tui.get_float timeout_s)
            ~max_retries:(Setup_tui.get_int max_retries)
            ~base_delay_s:(Setup_tui.get_float base_delay_s)
            ~fallback_provider:(Setup_tui.get_str fallback_provider));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
