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
  let fields =
    [
      ("bot_token", `String bot_token);
      ("signing_secret", `String signing_secret);
      ("events_path", `String events_path);
      ("allow_channels", `List (List.map (fun s -> `String s) allow_channels));
      ("allow_users", `List (List.map (fun s -> `String s) allow_users));
      ("app_token", `String app_token);
      ("socket_mode", `Bool socket_mode);
    ]
  in
  `Assoc [ ("channels", `Assoc [ ("slack", `Assoc fields) ]) ]

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
        \    set up a tunnel: clawq tunnel start\n"
    | _ -> "")

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.slack
  with _ -> None

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~(cfg : Runtime_config.slack_config) =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  draw_box ~width:w
    [
      bold " Slack Configuration ";
      "";
      Printf.sprintf "  Bot Token:       %s"
        (if cfg.bot_token = "" then dim "(not set)"
         else green (Tui_input.redact cfg.bot_token));
      Printf.sprintf "  Signing Secret:  %s"
        (if cfg.signing_secret = "" then dim "(not set)"
         else green (Tui_input.redact cfg.signing_secret));
      Printf.sprintf "  Events Path:     %s" cfg.events_path;
      Printf.sprintf "  Allow Channels:  %s"
        (String.concat ", " cfg.allow_channels);
      Printf.sprintf "  Allow Users:     %s"
        (String.concat ", " cfg.allow_users);
      Printf.sprintf "  App Token:       %s"
        (if cfg.app_token = "" then dim "(not set)"
         else green (Tui_input.redact cfg.app_token));
      Printf.sprintf "  Socket Mode:     %s"
        (if cfg.socket_mode then green "enabled" else dim "disabled");
      "";
    ];
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_slack_config ~(cfg : Runtime_config.slack_config) =
  let open Setup_common in
  let json =
    build_slack_json ~bot_token:cfg.bot_token ~signing_secret:cfg.signing_secret
      ~events_path:cfg.events_path ~allow_channels:cfg.allow_channels
      ~allow_users:cfg.allow_users ~app_token:cfg.app_token
      ~socket_mode:cfg.socket_mode
  in
  let full_json =
    match load_config_json () with
    | Some existing -> deep_merge_json existing json
    | None -> json
  in
  match write_config_json full_json with
  | Ok path ->
      print_success (Printf.sprintf "Saved to %s" path);
      true
  | Error e ->
      print_error (Printf.sprintf "Failed to write config: %s" e);
      false

(* ── Main menu loop ──────────────────────────────────────────────── *)

let default_slack_config : Runtime_config.slack_config =
  {
    bot_token = "";
    signing_secret = "";
    events_path = "/slack/events";
    allow_channels = [ "*" ];
    allow_users = [ "*" ];
    app_token = "";
    socket_mode = true;
  }

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let cfg =
        ref (match existing with Some c -> c | None -> default_slack_config)
      in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~cfg:!cfg;
        let options =
          [
            ("b", "Set bot token");
            ("s", "Set signing secret");
            ("e", "Set events path");
            ("c", "Set allowed channels");
            ("u", "Set allowed users");
            ("a", "Set app token");
            ( "m",
              Printf.sprintf "Toggle socket mode (%s)"
                (if !cfg.socket_mode then "currently on" else "currently off")
            );
            ("i", "Show setup instructions");
          ]
          @
          if !dirty then [ ("w", Setup_common.bold "Save configuration") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        match String.lowercase_ascii choice with
        | "q" | "" ->
            if !dirty then begin
              let save =
                Setup_common.prompt_yn
                  ~prompt:"You have unsaved changes. Save before exiting?"
                  ~default:true ()
              in
              if save then begin
                ignore (save_slack_config ~cfg:!cfg);
                quit := true
              end
              else quit := true
            end
            else quit := true
        | "b" -> (
            Printf.printf "\n";
            match Setup_common.prompt_secret ~prompt:"Slack Bot Token" () with
            | Ok tok -> (
                match validate_bot_token tok with
                | Ok t ->
                    cfg := { !cfg with bot_token = t };
                    dirty := true
                | Error e ->
                    Setup_common.print_error e;
                    Setup_common.press_enter_to_continue ())
            | Error e ->
                Setup_common.print_error e;
                Setup_common.press_enter_to_continue ())
        | "s" -> (
            Printf.printf "\n";
            match
              Setup_common.prompt_secret ~prompt:"Slack Signing Secret" ()
            with
            | Ok sec -> (
                match validate_signing_secret sec with
                | Ok s ->
                    cfg := { !cfg with signing_secret = s };
                    dirty := true
                | Error e ->
                    Setup_common.print_error e;
                    Setup_common.press_enter_to_continue ())
            | Error e ->
                Setup_common.print_error e;
                Setup_common.press_enter_to_continue ())
        | "e" ->
            let path =
              Setup_common.prompt_string ~prompt:"Events path"
                ~default:!cfg.events_path ()
            in
            cfg := { !cfg with events_path = path };
            dirty := true
        | "c" ->
            let current = String.concat "," !cfg.allow_channels in
            let input =
              Setup_common.prompt_string
                ~prompt:"Allowed channels (* = all, comma-separated)"
                ~default:current ()
            in
            let channels =
              String.split_on_char ',' input
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            let channels = if channels = [] then [ "*" ] else channels in
            cfg := { !cfg with allow_channels = channels };
            dirty := true
        | "u" ->
            let current = String.concat "," !cfg.allow_users in
            let input =
              Setup_common.prompt_string
                ~prompt:"Allowed users (* = all, comma-separated)"
                ~default:current ()
            in
            let users =
              String.split_on_char ',' input
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            let users = if users = [] then [ "*" ] else users in
            cfg := { !cfg with allow_users = users };
            dirty := true
        | "a" -> (
            Printf.printf "\n";
            match
              Setup_common.prompt_secret ~prompt:"Slack App Token (xapp-...)" ()
            with
            | Ok tok -> (
                match validate_app_token tok with
                | Ok t ->
                    cfg := { !cfg with app_token = t };
                    dirty := true
                | Error e ->
                    Setup_common.print_error e;
                    Setup_common.press_enter_to_continue ())
            | Error e ->
                Setup_common.print_error e;
                Setup_common.press_enter_to_continue ())
        | "m" ->
            cfg := { !cfg with socket_mode = not !cfg.socket_mode };
            dirty := true
        | "i" ->
            let rcfg =
              try Config_loader.load () with _ -> Runtime_config.default
            in
            let gateway_port = rcfg.gateway.port in
            let tunnel_url =
              if rcfg.tunnel.enabled && String.trim rcfg.tunnel.url <> "" then
                Some rcfg.tunnel.url
              else None
            in
            let instructions =
              post_setup_instructions ~events_path:!cfg.events_path
                ~socket_mode:!cfg.socket_mode ~gateway_port ~tunnel_url
            in
            Printf.printf "%s" instructions;
            Setup_common.press_enter_to_continue ()
        | "w" when !dirty ->
            if save_slack_config ~cfg:!cfg then dirty := false;
            Setup_common.press_enter_to_continue ()
        | s ->
            Setup_common.print_warning (Printf.sprintf "Unknown option: %s" s);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes." else "Slack setup complete."
