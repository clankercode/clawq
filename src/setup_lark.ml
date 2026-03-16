(* setup_lark.ml — Interactive setup wizard for Lark (Feishu) integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_app_id s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "App ID cannot be empty. Find it in Feishu/Lark Developer Console > your \
       app > Credentials & Basic Info (format: cli_xxxxxxxx)."
  else if String.length trimmed >= 4 && String.sub trimmed 0 4 = "cli_" then
    Ok trimmed
  else
    Error
      (Printf.sprintf
         "App ID must start with 'cli_' (got: %s). Find it in Developer \
          Console > Credentials & Basic Info."
         trimmed)

let validate_non_empty label hint s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error (Printf.sprintf "%s cannot be empty. %s" label hint)
  else Ok trimmed

let validate_app_secret s =
  validate_non_empty "App Secret"
    "Find it in Feishu/Lark Developer Console > your app > Credentials & Basic \
     Info."
    s

let validate_verification_token s =
  validate_non_empty "Verification Token"
    "Find it in Feishu/Lark Developer Console > your app > Event Subscriptions \
     (after setting the Request URL)."
    s

let build_lark_json ~enabled ~app_id ~app_secret ~verification_token ~endpoint
    ~mode ~allow_users =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "lark",
              `Assoc
                [
                  ("enabled", `Bool enabled);
                  ("app_id", `String app_id);
                  ("app_secret", `String app_secret);
                  ("verification_token", `String verification_token);
                  ("endpoint", `String endpoint);
                  ("mode", `String mode);
                  ( "allow_users",
                    `List (List.map (fun s -> `String s) allow_users) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete Lark (Feishu) bot setup:

    Developer console:
      Feishu (China):       https://open.feishu.cn/app
      Lark (International): https://open.larksuite.com/app

    1. Create a custom app:
         "Create App" > Custom App > fill name and description

    2. Collect credentials (Credentials & Basic Info):
         App ID      cli_xxxxxxxxxx
         App Secret  (click "Show" to reveal)

    3. Enable the Bot feature:
         App Features > Bot > enable

    4. Configure Event Subscriptions:
         Event Subscriptions > set Request URL to:
           https://your-server/lark/events   (or your configured endpoint path)
         Copy the Verification Token shown on the page.
         Subscribe to event: im.message.receive_v1

    5. Set permissions:
         Permissions & Scopes > add:
           im:message:send_as_bot
           im:message

    6. Publish and add to workspace:
         App Publishing > Publish (may require workspace admin approval)

    allow_users: Comma-separated Lark user open_ids or union_ids.
      Use * to allow all workspace members.

  After saving:
    - Start the daemon: clawq daemon start
    - Send a message to your bot in Lark/Feishu
    - Check daemon logs: clawq daemon logs

  Full documentation: https://clawq.org/channels/#lark
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.lark
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let enabled_field =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle enabled"
      ~description:"Enable or disable the Lark/Feishu channel integration."
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.enabled
        | None -> true)
      ()
  in
  let app_id_field =
    Setup_tui.make_field ~key:"d" ~label:"App ID" ~menu_label:"Set App ID"
      ~description:
        "Lark/Feishu App ID (format: cli_xxxxxxxx). Found in Developer Console \
         > Credentials & Basic Info."
      ~validate:validate_app_id
      ~default:
        (match existing with Some l -> l.Runtime_config.app_id | None -> "")
      ()
  in
  let app_secret_field =
    Setup_tui.make_secret_field ~key:"x" ~label:"App Secret"
      ~menu_label:"Set App Secret"
      ~description:
        "Lark/Feishu App Secret. Found in Developer Console > Credentials & \
         Basic Info (click Show to reveal)."
      ~validate:validate_app_secret
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.app_secret
        | None -> "")
      ()
  in
  let verification_token_field =
    Setup_tui.make_secret_field ~key:"v" ~label:"Verification Token"
      ~menu_label:"Set Verification Token"
      ~description:
        "Token used to verify event webhook requests. Found in Developer \
         Console > Event Subscriptions after setting the Request URL."
      ~validate:validate_verification_token
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.verification_token
        | None -> "")
      ()
  in
  let endpoint_field =
    Setup_tui.make_field ~key:"p" ~label:"Endpoint"
      ~menu_label:"Set webhook endpoint path"
      ~description:
        "HTTP path where clawq receives Lark events (e.g. /lark/events). Set \
         this as the Request URL suffix in Event Subscriptions."
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.endpoint
        | None -> "/lark/events")
      ()
  in
  let mode_field =
    Setup_tui.make_choice_field ~key:"m" ~label:"Mode" ~menu_label:"Set mode"
      ~choices:[ "event"; "webhook" ]
      ~description:
        "How clawq receives Lark events. 'event' = webhook push (recommended \
         for production); 'webhook' = pull-based webhook mode."
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.mode
        | None -> "event")
      ()
  in
  let allow_users_field =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow Users"
      ~menu_label:"Set allowed users"
      ~description:
        "Comma-separated Lark/Feishu user open_ids or union_ids allowed to \
         send commands. Use * to allow all workspace members."
      ~default:
        (match existing with
        | Some l -> l.Runtime_config.allow_users
        | None -> [ "*" ])
      ()
  in
  let fields =
    [
      enabled_field;
      app_id_field;
      app_secret_field;
      verification_token_field;
      endpoint_field;
      mode_field;
      allow_users_field;
    ]
  in
  let build_json () =
    build_lark_json
      ~enabled:(Setup_tui.get_bool enabled_field)
      ~app_id:(Setup_tui.get_str app_id_field)
      ~app_secret:(Setup_tui.get_str app_secret_field)
      ~verification_token:(Setup_tui.get_str verification_token_field)
      ~endpoint:(Setup_tui.get_str endpoint_field)
      ~mode:(Setup_tui.get_str mode_field)
      ~allow_users:(Setup_tui.get_str_list allow_users_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str app_id_field = "" then Error "App ID is required."
    else if Setup_tui.get_str app_secret_field = "" then
      Error "App Secret is required."
    else if Setup_tui.get_str verification_token_field = "" then
      Error "Verification Token is required."
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "Lark (Feishu) Channel Configuration";
      docs_url = "https://clawq.org/channels/#lark";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
