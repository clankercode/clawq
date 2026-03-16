(* setup_imessage.ml — Interactive setup wizard for iMessage channel *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_poll_interval s =
  match float_of_string_opt (String.trim s) with
  | Some v when v > 0.0 -> Ok (String.trim s)
  | Some _ ->
      Error "Poll interval must be greater than 0. Recommended: 5.0 seconds."
  | None -> Error "Poll interval must be a valid number (e.g. 5.0)."

let build_imessage_json ~poll_interval_s ~allow_from =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "imessage",
              `Assoc
                [
                  ("poll_interval_s", `Float poll_interval_s);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  iMessage channel setup (macOS only):

    Requirements:
      - macOS 10.15 (Catalina) or later
      - Messages.app signed in with an Apple ID
      - clawq must run as the same user that owns Messages.app

    Setup steps:
      1. Open System Settings > Privacy & Security > Automation
      2. Find your terminal app (Terminal, iTerm2, etc.) and enable
         access to "Messages"
      3. (First run) macOS may prompt to allow AppleScript access —
         click Allow
      4. Configure allow_from to restrict senders:
           *          — accept messages from anyone in your contacts
           +15551234567 — accept only from this number (E.164 format)
           user@icloud.com — accept only from this Apple ID

    After saving:
      - Start the daemon: clawq daemon start
      - Send an iMessage to the Apple ID clawq is monitoring
      - Check daemon logs: clawq daemon logs

    Troubleshooting:
      - If clawq cannot read messages, re-check Automation permissions
      - The poll_interval_s (default 5s) controls message latency

  Full documentation: https://clawq.org/channels/#imessage
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.imessage
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let poll_interval_s =
    Setup_tui.make_float_field ~key:"p" ~label:"Poll interval (s)"
      ~menu_label:"Set poll interval (seconds)"
      ~description:
        "How often (in seconds) to poll Messages.app for new messages. Lower \
         values reduce latency but increase CPU usage. Recommended: 5.0."
      ~validate:validate_poll_interval
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.poll_interval_s
        | None -> 5.0)
      ()
  in
  let allow_from =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow from"
      ~menu_label:"Set allowed senders"
      ~description:
        "Comma-separated phone numbers (E.164, e.g. +15551234567) or Apple IDs \
         (email). Use * to accept messages from all contacts."
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " iMessage Channel Configuration ";
      docs_url = "https://clawq.org/channels/#imessage";
      fields = [ poll_interval_s; allow_from ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_imessage_json
            ~poll_interval_s:(Setup_tui.get_float poll_interval_s)
            ~allow_from:(Setup_tui.get_str_list allow_from));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
