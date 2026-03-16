(* setup_dingtalk.ml — Interactive setup wizard for DingTalk channel *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_agent_id s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Agent ID cannot be empty. Find it in DingTalk Open Platform > your app \
       > App capabilities > Robot (numeric ID shown there)."
  else if
    String.length trimmed > 0
    && String.to_seq trimmed |> Seq.for_all (fun c -> c >= '0' && c <= '9')
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Agent ID must contain only digits (got: %s). Find the numeric Agent \
          ID in DingTalk Open Platform > App capabilities > Robot."
         trimmed)

let validate_non_empty label hint s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error (Printf.sprintf "%s cannot be empty. %s" label hint)
  else Ok trimmed

let validate_app_key s =
  validate_non_empty "App key"
    "Find it in DingTalk Open Platform > your app > Credentials and Basic Info."
    s

let validate_app_secret s =
  validate_non_empty "App secret"
    "Find it in DingTalk Open Platform > your app > Credentials and Basic Info."
    s

let validate_webhook_url s =
  let trimmed = String.trim s in
  if trimmed = "" then Ok ""
  else if
    (String.length trimmed >= 7 && String.sub trimmed 0 7 = "http://")
    || (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://")
  then Ok trimmed
  else
    Error
      "Webhook URL must start with http:// or https://, or leave empty to skip."

let build_dingtalk_json ~app_key ~app_secret ~agent_id ~allow_from ~webhook_url
    =
  let fields =
    [
      ("app_key", `String app_key);
      ("app_secret", `String app_secret);
      ("agent_id", `String agent_id);
      ("allow_from", `List (List.map (fun s -> `String s) allow_from));
    ]
  in
  let fields =
    match webhook_url with
    | Some url -> fields @ [ ("webhook_url", `String url) ]
    | None -> fields
  in
  `Assoc [ ("channels", `Assoc [ ("dingtalk", `Assoc fields) ]) ]

let post_setup_instructions =
  {|
  DingTalk channel setup:

    1. Go to: https://open.dingtalk.com/
    2. Click "Developer Platform" and sign in
    3. Create an internal application:
         My Apps > Create App > Internal App > Robot & Message

    4. Collect credentials from the app console:
         App Key     Credentials and Basic Info > AppKey
         App Secret  Credentials and Basic Info > AppSecret
         Agent ID    App capabilities > Robot > Agent ID (numeric)

    5. Configure the message receiving URL:
         App capabilities > Robot > Message Receiving Mode
         Set callback URL to: https://your-server/dingtalk/webhook

    6. Publish the app:
         App Publishing > Publish (internal apps need company admin approval)

    allow_from: Comma-separated DingTalk staffId values (internal user IDs).
      Use * to allow all organization members.

    webhook_url (optional): Only needed for proactive/outgoing messages.
      Leave empty if clawq only responds to incoming messages.

  After saving:
    - Start the daemon: clawq daemon start
    - @ mention the robot in a DingTalk group or send it a 1:1 message
    - Check daemon logs: clawq daemon logs

  Full documentation: https://clawq.org/channels/#dingtalk
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.dingtalk
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let app_key =
    Setup_tui.make_secret_field ~key:"k" ~label:"App key"
      ~menu_label:"Set app key"
      ~description:
        "DingTalk app key (AppKey). Found in Open Platform > your app > \
         Credentials and Basic Info."
      ~validate:validate_app_key
      ~default:
        (match existing with Some c -> c.Runtime_config.app_key | None -> "")
      ()
  in
  let app_secret =
    Setup_tui.make_secret_field ~key:"x" ~label:"App secret"
      ~menu_label:"Set app secret"
      ~description:
        "DingTalk app secret (AppSecret). Found in Open Platform > your app > \
         Credentials and Basic Info."
      ~validate:validate_app_secret
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.app_secret
        | None -> "")
      ()
  in
  let agent_id =
    Setup_tui.make_field ~key:"i" ~label:"Agent ID" ~menu_label:"Set agent ID"
      ~description:
        "Numeric agent ID for the DingTalk robot. Found in Open Platform > \
         your app > App capabilities > Robot."
      ~validate:validate_agent_id
      ~default:
        (match existing with Some c -> c.Runtime_config.agent_id | None -> "")
      ()
  in
  let allow_from =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow from"
      ~menu_label:"Set allowed senders"
      ~description:
        "Comma-separated DingTalk staffId values (internal user IDs) allowed \
         to send commands. Use * to allow all organization members."
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let webhook_url =
    Setup_tui.make_field ~key:"w" ~label:"Webhook URL (optional)"
      ~menu_label:"Set webhook URL (optional)"
      ~description:
        "Optional outgoing webhook URL for proactive (bot-initiated) messages \
         (http:// or https://). Leave empty if not needed."
      ~validate:validate_webhook_url
      ~default:
        (match existing with
        | Some c -> Option.value c.Runtime_config.webhook_url ~default:""
        | None -> "")
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " DingTalk Channel Configuration ";
      docs_url = "https://clawq.org/channels/#dingtalk";
      fields = [ app_key; app_secret; agent_id; allow_from; webhook_url ];
      extra_actions = [];
      build_json =
        (fun () ->
          let wh =
            let s = Setup_tui.get_str webhook_url in
            if s = "" then None else Some s
          in
          build_dingtalk_json
            ~app_key:(Setup_tui.get_str app_key)
            ~app_secret:(Setup_tui.get_str app_secret)
            ~agent_id:(Setup_tui.get_str agent_id)
            ~allow_from:(Setup_tui.get_str_list allow_from)
            ~webhook_url:wh);
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str app_key = "" then Error "App key is required."
          else if Setup_tui.get_str app_secret = "" then
            Error "App secret is required."
          else if Setup_tui.get_str agent_id = "" then
            Error "Agent ID is required."
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
