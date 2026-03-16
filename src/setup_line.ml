(* setup_line.ml — Interactive setup wizard for LINE channel *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_non_empty label s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      (Printf.sprintf
         "%s cannot be empty. Find it in the LINE Developers console." label)
  else Ok trimmed

let validate_channel_access_token s =
  validate_non_empty "Channel access token" s

let validate_channel_secret s = validate_non_empty "Channel secret" s

let build_line_json ~channel_access_token ~channel_secret ~allow_from =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "line",
              `Assoc
                [
                  ("channel_access_token", `String channel_access_token);
                  ("channel_secret", `String channel_secret);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  LINE Messaging API setup:

    1. Go to: https://developers.line.biz/console/
    2. Select your provider (or create one) then click "Create a channel"
    3. Choose "Messaging API" as the channel type
    4. Fill in the required details and create the channel

    Getting credentials:
      Channel access token:
        - Messaging API tab > Channel access token (long-lived)
        - Click "Issue" if no token exists yet
      Channel secret:
        - Basic settings tab > Channel secret

    Webhook setup:
      - Messaging API tab > Webhook settings
      - Set webhook URL to: https://your-server/line/webhook
      - Click "Verify" to test the connection
      - Enable "Use webhook"
      - Disable "Auto-reply messages" (prevents double-replies)

    allow_from: Comma-separated LINE user IDs (Uxxxxxxxxxx format)
      Use * to accept messages from any user

  After saving:
    - Start the daemon: clawq daemon start
    - Add your bot as a friend in LINE and send a message
    - Check daemon logs: clawq daemon logs

  Full documentation: https://clawq.org/channels/#line
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.line
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let channel_access_token =
    Setup_tui.make_secret_field ~key:"t" ~label:"Channel access token"
      ~menu_label:"Set channel access token"
      ~description:
        "Long-lived channel access token from LINE Developers console. Found \
         under: Messaging API tab > Channel access token."
      ~validate:validate_channel_access_token
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.channel_access_token
        | None -> "")
      ()
  in
  let channel_secret =
    Setup_tui.make_secret_field ~key:"c" ~label:"Channel secret"
      ~menu_label:"Set channel secret"
      ~description:
        "Channel secret used to verify webhook signatures. Found under: Basic \
         settings tab > Channel secret."
      ~validate:validate_channel_secret
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.channel_secret
        | None -> "")
      ()
  in
  let allow_from =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow from"
      ~menu_label:"Set allowed senders"
      ~description:
        "Comma-separated LINE user IDs (format: Uxxxxxxxxxx) allowed to \
         interact with the bot. Use * to allow all users."
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " LINE Channel Configuration ";
      docs_url = "https://clawq.org/channels/#line";
      fields = [ channel_access_token; channel_secret; allow_from ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_line_json
            ~channel_access_token:(Setup_tui.get_str channel_access_token)
            ~channel_secret:(Setup_tui.get_str channel_secret)
            ~allow_from:(Setup_tui.get_str_list allow_from));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str channel_access_token = "" then
            Error "Channel access token is required."
          else if Setup_tui.get_str channel_secret = "" then
            Error "Channel secret is required."
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
