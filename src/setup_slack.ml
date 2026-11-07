(* setup_slack.ml — Interactive setup wizard for Slack integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

(* Canonical app permissions; connections:write belongs to the app token. *)
let bot_scopes =
  [
    "app_mentions:read";
    "assistant:write";
    "channels:history";
    "channels:read";
    "chat:write";
    "groups:history";
    "groups:read";
    "im:history";
    "im:read";
    "im:write";
    "mpim:history";
    "mpim:read";
    "users:read";
    "files:read";
    "files:write";
    "canvases:read";
    "canvases:write";
    "reactions:read";
    "reactions:write";
  ]

let bot_events =
  [
    "app_mention";
    "message.channels";
    "message.groups";
    "message.im";
    "agent_session_stopped";
    "app_home_opened";
    "app_context_changed";
    "reaction_added";
    "member_joined_channel";
    "channel_shared";
  ]

(* HTTP URLs are editable examples, never an inferred public endpoint. *)
let manifest_json ~socket_mode : Yojson.Safe.t =
  let endpoint = `String "https://example.invalid/slack/events" in
  let http_url key = if socket_mode then [] else [ (key, endpoint) ] in
  `Assoc
    [
      ("display_information", `Assoc [ ("name", `String "Clawq") ]);
      ( "features",
        `Assoc
          [
            ( "bot_user",
              `Assoc
                [
                  ("display_name", `String "Clawq");
                  ("always_online", `Bool true);
                ] );
            ( "app_home",
              `Assoc
                [
                  ("home_tab_enabled", `Bool true);
                  ("messages_tab_enabled", `Bool true);
                  ("messages_tab_read_only", `Bool false);
                ] );
            ( "agent_view",
              `Assoc [ ("agent_description", `String "Clawq thread assistant") ]
            );
          ] );
      ( "oauth_config",
        `Assoc
          [
            ( "scopes",
              `Assoc [ ("bot", Setup_common.json_string_list bot_scopes) ] );
          ] );
      ( "settings",
        `Assoc
          [
            ( "event_subscriptions",
              `Assoc
                (("bot_events", Setup_common.json_string_list bot_events)
                :: http_url "request_url") );
            ( "interactivity",
              `Assoc (("is_enabled", `Bool true) :: http_url "request_url") );
            ("socket_mode_enabled", `Bool socket_mode);
          ] );
    ]

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

let validate_transport_credentials ~bot_token ~app_token ~signing_secret
    ~socket_mode =
  let valid s = Runtime_config.is_credential_valid (String.trim s) in
  if not (valid bot_token) then Error "A valid bot token is required."
  else if socket_mode && not (valid app_token) then
    Error "Socket Mode requires an app token (xapp-...) with connections:write."
  else if (not socket_mode) && not (valid signing_secret) then
    Error "HTTP Events requires a signing secret for request verification."
  else Ok ()

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
%s
       Thread context verifies the human and bot are channel members.
       Canvas/file permissions support long answers and I065 canvas tools.
       Huddle reads require bot and requesting-user channel membership.
    Agent experience (current agent_view):
       Declare features.bot_user and features.app_home with writable
       messages, plus features.agent_view with an agent_description
       (for example, "Clawq thread assistant").
       Subscribe to the complete bot event list below (native task UX,
       channel messages, pickup reactions and channel posture changes).
       Enable Interactivity for the run-specific "Stop this task" buttons.
       Socket Mode delivers interactive envelopes; HTTP uses the same signed
       Events Request URL for Interactivity. Native APIs may return
       feature_disabled; Clawq then uses ordinary threaded progress/buttons.
       Legacy assistant_view events are not implemented by this connector.
    6. Install the app to your workspace
    7. Copy the "Bot User OAuth Token" (xoxb-...) into this wizard
%s%s|}
    (String.concat "\n"
       (List.map (fun scope -> "       - " ^ scope) bot_scopes))
    (if socket_mode then
       Printf.sprintf
         {|
    Socket Mode (recommended):
    8. Navigate to "Settings" > "Socket Mode"
    9. Enable Socket Mode
   10. Generate an App-Level Token with "connections:write" scope
   11. Copy the token (xapp-...) into this wizard

    Event Subscriptions:
   12. Navigate to "Event Subscriptions"
   13. Enable Events (no Request URL needed in Socket Mode)
   14. Subscribe to bot events: %s
|}
         (String.concat ", " bot_events)
     else
       Printf.sprintf
         {|
    Event Subscriptions:
    8. Navigate to "Event Subscriptions"
    9. Enable Events
   10. Set Request URL to: %s
   11. Subscribe to bot events: %s

    Signing Secret:
   12. Navigate to "Settings" > "Basic Information"
   13. Copy the "Signing Secret" into this wizard
|}
         events_url
         (String.concat ", " bot_events))
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
    allow_private_channels = [];
    private_channel_policy = Runtime_config.Pc_deny;
    app_token = "";
    socket_mode = true;
    default_model = None;
    event_log = Slack_config_types.default_event_log;
    long_reply = Slack_config_types.default_long_reply;
    reply_mode = Slack_config_types.Mention;
    standing_orders = Slack_config_types.default_standing_orders;
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
        "Required for HTTP Events and Interactivity; optional in Socket Mode. \
         Found in Basic Information."
      ~validate:(fun input ->
        if String.trim input = "" then Ok "" else validate_signing_secret input)
      ~default:d.signing_secret ()
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
          validate_transport_credentials
            ~bot_token:(Setup_tui.get_str bot_token)
            ~app_token:(Setup_tui.get_str app_token)
            ~signing_secret:(Setup_tui.get_str signing_secret)
            ~socket_mode:(Setup_tui.get_bool socket_mode));
      post_instructions = live_instructions;
    }
  in
  Setup_tui.run_wizard spec
