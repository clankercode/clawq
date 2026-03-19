(* setup_slack.ml — Interactive setup wizard for Slack integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_bot_token s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Bot token cannot be empty."
  else if String.length trimmed >= 5 && String.sub trimmed 0 5 = "xoxb-" then
    Ok trimmed
  else Error "Bot token must start with 'xoxb-'."

let validate_signing_secret s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Signing secret cannot be empty." else Ok trimmed

let validate_app_token s =
  let trimmed = String.trim s in
  if trimmed = "" then Ok trimmed
  else if String.length trimmed >= 5 && String.sub trimmed 0 5 = "xapp-" then
    Ok trimmed
  else Error "App token must start with 'xapp-' (or leave empty)."

let build_slack_json ~bot_token ~signing_secret ~events_path ~allow_channels
    ~allow_users ~app_token ~socket_mode =
  Setup_common.build_channel_json ~channel_name:"slack"
    [
      ("bot_token", `String bot_token);
      ("signing_secret", `String signing_secret);
      ("events_path", `String events_path);
      ("allow_channels", Setup_common.json_string_list allow_channels);
      ("allow_users", Setup_common.json_string_list allow_users);
      ("app_token", `String app_token);
      ("socket_mode", `Bool socket_mode);
    ]

let post_setup_instructions ~events_path ~socket_mode ~gateway_port ~tunnel_url
    =
  let base_url =
    match tunnel_url with
    | Some url -> url
    | None -> Printf.sprintf "http://localhost:%d" gateway_port
  in
  let events_url = base_url ^ events_path in
  Printf.sprintf
    {|
  Complete Slack app setup:

    1. Go to: https://api.slack.com/apps
    2. Click "Create New App" > "From scratch"
    3. Name your app and select your workspace

    OAuth & Permissions:
    4. Navigate to "OAuth & Permissions"
    5. Add these Bot Token Scopes:
       - app_mentions:read
       - chat:write
       - channels:history
       - groups:history
       - im:history
       - mpim:history
    6. Install the app to your workspace
    7. Copy the "Bot User OAuth Token" (xoxb-...) into this wizard
%s%s|}
    (if socket_mode then
       {|
    Socket Mode (recommended):
    8. Navigate to "Settings" > "Socket Mode"
    9. Enable Socket Mode
   10. Generate an App-Level Token with "connections:write" scope
   11. Copy the token (xapp-...) into this wizard

    Event Subscriptions:
   12. Navigate to "Event Subscriptions"
   13. Enable Events (no Request URL needed in Socket Mode)
   14. Subscribe to bot events: app_mention, message.channels, message.groups, message.im
|}
     else
       Printf.sprintf
         {|
    Event Subscriptions:
    8. Navigate to "Event Subscriptions"
    9. Enable Events
   10. Set Request URL to: %s
   11. Subscribe to bot events: app_mention, message.channels, message.groups, message.im

    Signing Secret:
   12. Navigate to "Settings" > "Basic Information"
   13. Copy the "Signing Secret" into this wizard
|}
         events_url)
    (match tunnel_url with
    | None when not socket_mode ->
        "\n\
        \    Note: You are using localhost. For Slack to reach your server,\n\
        \    set up a tunnel: clawq tunnel start\n\n\
        \  Full documentation: https://clawq.org/channels/#slack\n"
    | _ -> "\n  Full documentation: https://clawq.org/channels/#slack\n")

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  Setup_common.load_config_opt (fun cfg -> cfg.channels.slack)

(* ── Main wizard ─────────────────────────────────────────────────── *)

let default_slack_config : Runtime_config.slack_config =
  {
    bot_token = "";
    signing_secret = "";
    events_path = "/slack/events";
    allow_channels = [ "*" ];
    allow_users = [ "*" ];
    app_token = "";
    socket_mode = true;
    default_model = None;
  }

let run () =
  let existing = load_existing () in
  let d = match existing with Some c -> c | None -> default_slack_config in
  let bot_token =
    Setup_tui.make_secret_field ~key:"b" ~label:"Bot Token"
      ~menu_label:"Set bot token"
      ~description:"Slack Bot User OAuth Token (starts with xoxb-)."
      ~validate:validate_bot_token ~default:d.bot_token ()
  in
  let signing_secret =
    Setup_tui.make_secret_field ~key:"s" ~label:"Signing Secret"
      ~menu_label:"Set signing secret"
      ~description:
        "Slack signing secret for verifying webhook requests. Found in Basic \
         Information."
      ~validate:validate_signing_secret ~default:d.signing_secret ()
  in
  let events_path =
    Setup_tui.make_field ~key:"e" ~label:"Events Path"
      ~menu_label:"Set events path"
      ~description:"URL path for Slack event subscriptions."
      ~default:d.events_path ()
  in
  let allow_channels =
    Setup_tui.make_list_field ~key:"c" ~label:"Allow Channels"
      ~menu_label:"Set allowed channels"
      ~description:"Comma-separated Slack channel IDs, or * for all."
      ~default:d.allow_channels ()
  in
  let allow_users =
    Setup_tui.make_list_field ~key:"u" ~label:"Allow Users"
      ~menu_label:"Set allowed users"
      ~description:"Comma-separated Slack user IDs, or * for all."
      ~default:d.allow_users ()
  in
  let app_token =
    Setup_tui.make_secret_field ~key:"a" ~label:"App Token"
      ~menu_label:"Set app token"
      ~description:
        "Slack App-Level Token (starts with xapp-). Required for Socket Mode."
      ~validate:validate_app_token ~default:d.app_token ()
  in
  let socket_mode =
    Setup_tui.make_bool_field ~key:"m" ~label:"Socket Mode"
      ~menu_label:"Toggle socket mode"
      ~description:
        "Use WebSocket connection instead of HTTP webhooks. Recommended — no \
         public URL needed."
      ~default:d.socket_mode ()
  in
  let live_instructions () =
    let gateway_port, tunnel_url = Setup_common.get_gateway_and_tunnel_url () in
    post_setup_instructions
      ~events_path:(Setup_tui.get_str events_path)
      ~socket_mode:(Setup_tui.get_bool socket_mode)
      ~gateway_port ~tunnel_url
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Slack Configuration ";
      docs_url = "https://clawq.org/channels/#slack";
      fields =
        [
          bot_token;
          signing_secret;
          events_path;
          allow_channels;
          allow_users;
          app_token;
          socket_mode;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_slack_json
            ~bot_token:(Setup_tui.get_str bot_token)
            ~signing_secret:(Setup_tui.get_str signing_secret)
            ~events_path:(Setup_tui.get_str events_path)
            ~allow_channels:(Setup_tui.get_str_list allow_channels)
            ~allow_users:(Setup_tui.get_str_list allow_users)
            ~app_token:(Setup_tui.get_str app_token)
            ~socket_mode:(Setup_tui.get_bool socket_mode));
      pre_save_check =
        (fun () ->
          Setup_tui.check_required_str_fields
            [ (bot_token, "Bot token is required.") ]);
      post_instructions = live_instructions;
    }
  in
  Setup_tui.run_wizard spec
