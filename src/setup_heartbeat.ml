(* setup_heartbeat.ml — Interactive setup wizard for heartbeat configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_interval s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Interval must be a positive integer."
  | None -> Error "Interval must be a valid integer."

let validate_hour s =
  match int_of_string_opt s with
  | Some v when v >= 0 && v <= 23 -> Ok s
  | Some _ -> Error "Hour must be between 0 and 23."
  | None -> Error "Hour must be a valid integer."

let build_heartbeat_json ~enabled ~interval_seconds ~quiet_start ~quiet_end =
  `Assoc
    [
      ( "heartbeat",
        `Assoc
          [
            ("enabled", `Bool enabled);
            ("interval_seconds", `Int interval_seconds);
            ("quiet_start", `Int quiet_start);
            ("quiet_end", `Int quiet_end);
          ] );
    ]

let post_setup_instructions =
  {|
  Heartbeat configuration setup:

    1. enabled: Enable periodic heartbeat messages to connected channels.
       Default: true.
    2. interval_seconds: How often (in seconds) to send heartbeat messages.
       Default: 250.
    3. quiet_start: Hour (0-23) when the quiet period starts. Heartbeats are
       suppressed during the quiet window. Default: 23 (11 PM).
    4. quiet_end: Hour (0-23) when the quiet period ends. Default: 8 (8 AM).

  The quiet window suppresses heartbeats between quiet_start and quiet_end.
  Hours use 24-hour format (0=midnight, 23=11 PM).

  After saving:

    - Restart the daemon: clawq daemon restart
    - Verify: clawq status

  Full documentation: https://clawq.org/heartbeat/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    Some cfg.heartbeat
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let default = Runtime_config.default.heartbeat in
  let get_h f = match existing with Some c -> f c | None -> f default in
  let enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle heartbeat"
      ~description:"Enable periodic heartbeat messages to connected channels."
      ~default:(get_h (fun c -> c.Runtime_config.enabled))
      ()
  in
  let interval_seconds =
    Setup_tui.make_int_field ~key:"i" ~label:"Interval (seconds)"
      ~menu_label:"Set heartbeat interval (seconds)"
      ~description:"How often to send heartbeat messages. Must be > 0."
      ~validate:validate_interval
      ~default:(get_h (fun c -> c.Runtime_config.interval_seconds))
      ()
  in
  let quiet_start =
    Setup_tui.make_int_field ~key:"qs" ~label:"Quiet start (hour 0-23)"
      ~menu_label:"Set quiet period start hour"
      ~description:
        "Hour when quiet period begins (heartbeats suppressed). 0-23."
      ~validate:validate_hour
      ~default:(get_h (fun c -> c.Runtime_config.quiet_start))
      ()
  in
  let quiet_end =
    Setup_tui.make_int_field ~key:"qe" ~label:"Quiet end (hour 0-23)"
      ~menu_label:"Set quiet period end hour"
      ~description:"Hour when quiet period ends (heartbeats resume). 0-23."
      ~validate:validate_hour
      ~default:(get_h (fun c -> c.Runtime_config.quiet_end))
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Heartbeat Configuration ";
      docs_url = "https://clawq.org/heartbeat/";
      fields = [ enabled; interval_seconds; quiet_start; quiet_end ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_heartbeat_json
            ~enabled:(Setup_tui.get_bool enabled)
            ~interval_seconds:(Setup_tui.get_int interval_seconds)
            ~quiet_start:(Setup_tui.get_int quiet_start)
            ~quiet_end:(Setup_tui.get_int quiet_end));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
