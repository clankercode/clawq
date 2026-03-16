(* setup_onebot.ml — Interactive setup wizard for OneBot integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_ws_url s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "WebSocket URL cannot be empty. Set it to your OneBot WebSocket server \
       URL (e.g. ws://localhost:8080 for go-cqhttp)."
  else if
    (String.length trimmed >= 6 && String.sub trimmed 0 6 = "wss://")
    || (String.length trimmed >= 5 && String.sub trimmed 0 5 = "ws://")
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "WebSocket URL must start with ws:// or wss:// (got: %s). Example: \
          ws://localhost:8080"
         trimmed)

let validate_http_url s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "HTTP URL cannot be empty. Set it to your OneBot HTTP API URL (e.g. \
       http://localhost:5700 for go-cqhttp)."
  else if
    (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://")
    || (String.length trimmed >= 7 && String.sub trimmed 0 7 = "http://")
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "HTTP URL must start with http:// or https:// (got: %s). Example: \
          http://localhost:5700"
         trimmed)

let build_onebot_json ~ws_url ~http_url ~access_token ~allow_from ~allow_groups
    =
  let access_token_json =
    match access_token with "" -> `Null | s -> `String s
  in
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "onebot",
              `Assoc
                [
                  ("ws_url", `String ws_url);
                  ("http_url", `String http_url);
                  ("access_token", access_token_json);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                  ( "allow_groups",
                    `List (List.map (fun s -> `String s) allow_groups) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  OneBot v11 channel setup:

    OneBot is a standard protocol for QQ bots. Popular implementations:
      go-cqhttp  https://github.com/Mrs4s/go-cqhttp  (mature, widely used)
      LLOneBot   https://github.com/LLOneBot/LLOneBot (QQNT-based)
      NapCat     https://github.com/NapNeko/NapCatQQ  (QQNT-based)

    1. Install one of the implementations above and sign in with a QQ account

    2. Configure the WebSocket server (for go-cqhttp, edit config.yml):
         ws_url:    The "universal" or "forward" WebSocket address
                    Default: ws://localhost:8080
         http_url:  The HTTP API server address
                    Default: http://localhost:5700

    3. (Optional) Set an access_token in your OneBot config for security.
       Use the same value in clawq's access_token field.

    4. Configure allow_from: QQ user IDs (numeric) allowed to send commands.
       Use * to allow all users.

    5. Configure allow_groups: QQ group IDs the bot responds in.
       Use * to allow all groups.
       Leave allow_groups empty to only respond to direct messages.

  After saving:
    - Start the daemon: clawq daemon start
    - Send a direct QQ message to the bot account
    - Check daemon logs: clawq daemon logs

  Full documentation: https://clawq.org/channels/#onebot
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.onebot
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let ws_url_field =
    Setup_tui.make_field ~key:"w" ~label:"WebSocket URL"
      ~menu_label:"Set WebSocket URL"
      ~description:
        "OneBot WebSocket server URL for receiving events (ws:// or wss://). \
         go-cqhttp default: ws://localhost:8080"
      ~validate:validate_ws_url
      ~default:
        (match existing with Some o -> o.Runtime_config.ws_url | None -> "")
      ()
  in
  let http_url_field =
    Setup_tui.make_field ~key:"u" ~label:"HTTP URL"
      ~menu_label:"Set HTTP API URL"
      ~description:
        "OneBot HTTP API URL for sending messages (http:// or https://). \
         go-cqhttp default: http://localhost:5700"
      ~validate:validate_http_url
      ~default:
        (match existing with Some o -> o.Runtime_config.http_url | None -> "")
      ()
  in
  let access_token_field =
    Setup_tui.make_secret_field ~key:"t" ~label:"Access Token"
      ~menu_label:"Set access token (optional)"
      ~description:
        "OneBot access token for authentication (optional). Must match the \
         access_token set in your OneBot implementation's config. Leave empty \
         if your OneBot server has no access token configured."
      ~default:
        (match existing with
        | Some o -> Option.value ~default:"" o.Runtime_config.access_token
        | None -> "")
      ()
  in
  let allow_from_field =
    Setup_tui.make_list_field ~key:"f" ~label:"Allow From"
      ~menu_label:"Set allowed user IDs"
      ~description:
        "Comma-separated QQ user IDs (numeric) allowed to send commands. Use * \
         to allow all users."
      ~default:
        (match existing with
        | Some o -> o.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let allow_groups_field =
    Setup_tui.make_list_field ~key:"g" ~label:"Allow Groups"
      ~menu_label:"Set allowed group IDs"
      ~description:
        "Comma-separated QQ group IDs (numeric) in which the bot responds. Use \
         * to allow all groups. Leave empty to only respond in direct \
         messages."
      ~default:
        (match existing with
        | Some o -> o.Runtime_config.allow_groups
        | None -> [ "*" ])
      ()
  in
  let fields =
    [
      ws_url_field;
      http_url_field;
      access_token_field;
      allow_from_field;
      allow_groups_field;
    ]
  in
  let build_json () =
    build_onebot_json
      ~ws_url:(Setup_tui.get_str ws_url_field)
      ~http_url:(Setup_tui.get_str http_url_field)
      ~access_token:(Setup_tui.get_str access_token_field)
      ~allow_from:(Setup_tui.get_str_list allow_from_field)
      ~allow_groups:(Setup_tui.get_str_list allow_groups_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str ws_url_field = "" then
      Error "WebSocket URL is required."
    else if Setup_tui.get_str http_url_field = "" then
      Error "HTTP URL is required."
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "OneBot Channel Configuration";
      docs_url = "https://clawq.org/channels/#onebot";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
