(* setup_notify.ml — Interactive setup wizard for notify configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let valid_channels = [ "telegram"; "discord"; "slack"; "email" ]

let validate_channel s =
  if List.mem s valid_channels then Ok s
  else
    Error
      (Printf.sprintf "Channel must be one of: %s."
         (String.concat ", " valid_channels))

let validate_target s =
  if String.trim s = "" then Error "Target must not be empty." else Ok s

let build_notify_json ~channel ~target =
  `Assoc
    [
      ( "notify",
        `Assoc [ ("channel", `String channel); ("target", `String target) ] );
    ]

let post_setup_instructions =
  {|
  Notify setup:

    Notifications are sent when cron jobs complete, errors are detected,
    or other system events occur that require your attention.

    channel: The channel used to deliver notifications.
      - telegram: Sends to a Telegram chat. Use your chat ID as the target.
      - discord:  Sends to a Discord channel. Use the channel ID as the target.
      - slack:    Sends to a Slack channel. Use the channel ID as the target.
      - email:    Sends an email. Use the recipient address as the target.

    target: The destination identifier for the chosen channel.

  After saving:

    - Start the daemon: clawq daemon start
    - Trigger a cron job or error to test that notifications arrive.

  Full documentation: https://clawq.org/notify/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () = try (Config_loader.load ()).notify with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let channel =
    Setup_tui.make_choice_field ~key:"c" ~label:"Channel"
      ~menu_label:"Set notification channel" ~choices:valid_channels
      ~description:
        "The channel used to deliver notifications (telegram, discord, slack, \
         email)."
      ~validate:validate_channel
      ~default:
        (match existing with
        | Some n -> n.Runtime_config.channel
        | None -> "telegram")
      ()
  in
  let target =
    Setup_tui.make_field ~key:"t" ~label:"Target"
      ~menu_label:"Set notification target"
      ~description:
        "Destination for the chosen channel. E.g. Telegram chat ID, Discord \
         channel ID, Slack channel ID, or email address."
      ~validate:validate_target
      ~default:
        (match existing with Some n -> n.Runtime_config.target | None -> "")
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Notify Configuration ";
      docs_url = "https://clawq.org/notify/";
      fields = [ channel; target ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_notify_json
            ~channel:(Setup_tui.get_str channel)
            ~target:(Setup_tui.get_str target));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
