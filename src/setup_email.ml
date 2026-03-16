(* setup_email.ml — Interactive setup wizard for email integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_port s =
  let trimmed = String.trim s in
  match int_of_string_opt trimmed with
  | None ->
      Error
        (Printf.sprintf
           "Port must be a valid integer, got: %s. IMAP default: 993, SMTP \
            default: 587."
           trimmed)
  | Some n when n < 1 || n > 65535 ->
      Error
        (Printf.sprintf
           "Port must be between 1 and 65535, got %d. IMAP default: 993, SMTP \
            default: 587."
           n)
  | Some _ -> Ok trimmed

let validate_email s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Email address cannot be empty. Example: bot@gmail.com or bot@example.com"
  else if not (String.contains trimmed '@') then
    Error
      (Printf.sprintf
         "Email address must contain '@', got: %s. Example: bot@example.com"
         trimmed)
  else
    let parts = String.split_on_char '@' trimmed in
    match List.rev parts with
    | domain :: _ when not (String.contains domain '.') ->
        Error
          (Printf.sprintf
             "Email address domain must contain '.', got: %s. Example: \
              bot@example.com"
             trimmed)
    | _ -> Ok trimmed

let validate_poll_interval s =
  let trimmed = String.trim s in
  match float_of_string_opt trimmed with
  | None ->
      Error
        (Printf.sprintf
           "Poll interval must be a valid number, got: %s. Example: 60 (check \
            every 60 seconds)."
           trimmed)
  | Some f when f <= 0.0 ->
      Error
        (Printf.sprintf
           "Poll interval must be greater than 0, got: %g. Minimum \
            recommended: 30 seconds."
           f)
  | Some _ -> Ok trimmed

let build_email_json ~imap_host ~imap_port ~smtp_host ~smtp_port ~username
    ~password ~from_address ~allow_from ~poll_interval_s =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "email",
              `Assoc
                [
                  ("imap_host", `String imap_host);
                  ("imap_port", `Int imap_port);
                  ("smtp_host", `String smtp_host);
                  ("smtp_port", `Int smtp_port);
                  ("username", `String username);
                  ("password", `String password);
                  ("from_address", `String from_address);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                  ("poll_interval_s", `Float poll_interval_s);
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete email channel setup:

    1. Enable IMAP access for your email account:
         Gmail:   Settings (gear) > See all settings > Forwarding and POP/IMAP > Enable IMAP
         Outlook: Settings > Mail > Sync email > enable IMAP access

    2. Generate an app-specific password (required when 2FA is enabled):
         Gmail:   https://myaccount.google.com/apppasswords
                  Account > Security > App passwords > create new app password
         Outlook: https://account.microsoft.com/security
                  Advanced security > App passwords

    3. IMAP server settings:
         Gmail:   imap.gmail.com   port 993  (TLS)
         Outlook: outlook.office365.com  port 993  (TLS)
         Yahoo:   imap.mail.yahoo.com  port 993  (TLS)
         Custom:  Use your provider's IMAP hostname and port.

    4. SMTP server settings (for sending replies):
         Gmail:   smtp.gmail.com   port 587  (STARTTLS)
         Outlook: smtp.office365.com  port 587  (STARTTLS)
         Yahoo:   smtp.mail.yahoo.com  port 587  (STARTTLS)

    5. Set username to your full email address (e.g. bot@gmail.com).
       Set password to your app-specific password (not your account password).
       Set from_address to the address the bot will send from.

    6. Configure allow_from to restrict which sender addresses are accepted:
         *  Accept email from any sender.
         alice@example.com,bob@example.com  Restrict to specific senders.

    7. Set poll_interval_s to how often the bot checks for new mail (seconds):
         60   Check every minute (default, reasonable for most use cases).
         300  Check every 5 minutes (more conservative).

    8. Start the daemon: clawq daemon start

  Common issues:
    - "Authentication failed": use an app-specific password, not your account password.
    - Gmail blocks sign-in: enable "Less secure app access" or use an App Password.
    - Emails not received: verify IMAP is enabled and allow_from includes sender.

  Full documentation: https://clawq.org/channels/#email
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.email
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let imap_host_field =
    Setup_tui.make_field ~key:"1" ~label:"IMAP Host" ~menu_label:"Set IMAP host"
      ~description:
        "IMAP server hostname for receiving emails. Gmail: imap.gmail.com, \
         Outlook: outlook.office365.com"
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.imap_host
        | None -> "")
      ()
  in
  let imap_port_field =
    Setup_tui.make_int_field ~key:"2" ~label:"IMAP Port"
      ~menu_label:"Set IMAP port"
      ~description:
        "IMAP server port. Default: 993 (TLS). Use 143 for STARTTLS (not \
         recommended)."
      ~validate:validate_port
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.imap_port
        | None -> 993)
      ()
  in
  let smtp_host_field =
    Setup_tui.make_field ~key:"3" ~label:"SMTP Host" ~menu_label:"Set SMTP host"
      ~description:
        "SMTP server hostname for sending replies. Gmail: smtp.gmail.com, \
         Outlook: smtp.office365.com"
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.smtp_host
        | None -> "")
      ()
  in
  let smtp_port_field =
    Setup_tui.make_int_field ~key:"4" ~label:"SMTP Port"
      ~menu_label:"Set SMTP port"
      ~description:
        "SMTP server port. Default: 587 (STARTTLS). Some servers use 465 (TLS)."
      ~validate:validate_port
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.smtp_port
        | None -> 587)
      ()
  in
  let username_field =
    Setup_tui.make_field ~key:"u" ~label:"Username"
      ~menu_label:"Set email username"
      ~description:
        "Email account username, usually the full email address, e.g. \
         bot@gmail.com"
      ~default:
        (match existing with Some e -> e.Runtime_config.username | None -> "")
      ()
  in
  let password_field =
    Setup_tui.make_secret_field ~key:"p" ~label:"Password"
      ~menu_label:"Set email password"
      ~description:
        "Email password or app-specific password. For Gmail/Outlook with 2FA, \
         use an App Password — not your account password."
      ~default:
        (match existing with Some e -> e.Runtime_config.password | None -> "")
      ()
  in
  let from_address_field =
    Setup_tui.make_field ~key:"f" ~label:"From Address"
      ~menu_label:"Set from address"
      ~description:
        "Email address the bot will use as the sender. Must match the \
         authenticated account, e.g. bot@gmail.com"
      ~validate:validate_email
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.from_address
        | None -> "")
      ()
  in
  let allow_from_field =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow From"
      ~menu_label:"Set allowed sender addresses"
      ~description:
        "Comma-separated email addresses allowed to send commands, or * for \
         all. Example: alice@example.com,bob@example.com"
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let poll_interval_field =
    Setup_tui.make_float_field ~key:"i" ~label:"Poll Interval (s)"
      ~menu_label:"Set poll interval"
      ~description:
        "How often to check for new emails, in seconds. Default: 60. Minimum \
         recommended: 30 (to avoid rate limiting)."
      ~validate:validate_poll_interval
      ~default:
        (match existing with
        | Some e -> e.Runtime_config.poll_interval_s
        | None -> 60.0)
      ()
  in
  let fields =
    [
      imap_host_field;
      imap_port_field;
      smtp_host_field;
      smtp_port_field;
      username_field;
      password_field;
      from_address_field;
      allow_from_field;
      poll_interval_field;
    ]
  in
  let build_json () =
    build_email_json
      ~imap_host:(Setup_tui.get_str imap_host_field)
      ~imap_port:(Setup_tui.get_int imap_port_field)
      ~smtp_host:(Setup_tui.get_str smtp_host_field)
      ~smtp_port:(Setup_tui.get_int smtp_port_field)
      ~username:(Setup_tui.get_str username_field)
      ~password:(Setup_tui.get_str password_field)
      ~from_address:(Setup_tui.get_str from_address_field)
      ~allow_from:(Setup_tui.get_str_list allow_from_field)
      ~poll_interval_s:(Setup_tui.get_float poll_interval_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str imap_host_field = "" then
      Error "IMAP host is required."
    else if Setup_tui.get_str smtp_host_field = "" then
      Error "SMTP host is required."
    else if Setup_tui.get_str username_field = "" then
      Error "Username is required."
    else if Setup_tui.get_str password_field = "" then
      Error "Password is required."
    else if Setup_tui.get_str from_address_field = "" then
      Error "From address is required."
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "Email Channel Configuration";
      docs_url = "https://clawq.org/channels/#email";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
