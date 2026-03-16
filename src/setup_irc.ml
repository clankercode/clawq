(* setup_irc.ml — Interactive setup wizard for IRC integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_port s =
  let trimmed = String.trim s in
  match int_of_string_opt trimmed with
  | None ->
      Error
        (Printf.sprintf
           "Port must be a valid integer, got: %s. Use 6697 for TLS or 6667 \
            for plaintext."
           trimmed)
  | Some n when n < 1 || n > 65535 ->
      Error
        (Printf.sprintf
           "Port must be between 1 and 65535, got %d. Use 6697 for TLS or 6667 \
            for plaintext."
           n)
  | Some _ -> Ok trimmed

let validate_nick s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Nick cannot be empty. Choose a unique IRC nickname for your bot, e.g. \
       clawqbot."
  else if String.to_seq trimmed |> Seq.exists (fun c -> c = ' ' || c = '\t')
  then
    Error
      (Printf.sprintf
         "Nick must not contain spaces, got: %S. IRC nicks cannot have \
          whitespace."
         trimmed)
  else Ok trimmed

let build_irc_json ~host ~port ~tls ~nick ~password ~sasl ~channels ~allow_from
    =
  let password_json = match password with "" -> `Null | s -> `String s in
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "irc",
              `Assoc
                [
                  ("host", `String host);
                  ("port", `Int port);
                  ("tls", `Bool tls);
                  ("nick", `String nick);
                  ("password", password_json);
                  ("sasl", `Bool sasl);
                  ("channels", `List (List.map (fun s -> `String s) channels));
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete IRC bot setup:

    1. Choose an IRC network and server:
         Libera.Chat:  irc.libera.chat  port 6697 (TLS)
         OFTC:         irc.oftc.net     port 6697 (TLS)
         EFnet:        irc.efnet.org    port 6697 (TLS)

    2. Register a nick for your bot with NickServ (most networks require this):
         /msg NickServ REGISTER <password> <email>
         /msg NickServ VERIFY REGISTER <nick> <code>   (if email verification required)

    3. Configure TLS (strongly recommended):
         TLS enabled:   port 6697
         TLS disabled:  port 6667 (plaintext — avoid on public networks)

    4. Configure authentication:
         SASL (recommended): set sasl: true and password to your NickServ password.
         NickServ only:      set sasl: false and password to your NickServ password.
         No auth:            leave password empty and sasl: false.

    5. Add channels the bot should auto-join:
         Channel names must include the '#' prefix, e.g. #mychannel,#another.
         The bot will join these channels on every connect.

    6. Configure allow_from to restrict which nicks can send commands:
         *  Accept from any nick.
         alice,bob  Accept only from these nicks.
         Note: IRC nicks are not authenticated by default; use NickServ-confirmed
         nicks or SASL for security-sensitive bots.

    7. Start the daemon: clawq daemon start

  Common issues:
    - "Nick already in use": choose a different nick or register it with NickServ.
    - SASL auth failure: verify password matches NickServ registration.
    - TLS connection error: try disabling TLS temporarily to confirm host is reachable.

  Full documentation: https://clawq.org/channels/#irc
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.irc
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let host_field =
    Setup_tui.make_field ~key:"o" ~label:"Host"
      ~menu_label:"Set IRC server host"
      ~description:"IRC server hostname, e.g. irc.libera.chat or irc.oftc.net"
      ~default:
        (match existing with Some c -> c.Runtime_config.host | None -> "")
      ()
  in
  let port_field =
    Setup_tui.make_int_field ~key:"p" ~label:"Port" ~menu_label:"Set IRC port"
      ~description:
        "IRC server port. 6697 for TLS (recommended), 6667 for plaintext."
      ~validate:validate_port
      ~default:
        (match existing with Some c -> c.Runtime_config.port | None -> 6697)
      ()
  in
  let tls_field =
    Setup_tui.make_bool_field ~key:"l" ~label:"TLS" ~menu_label:"Toggle TLS"
      ~description:
        "Enable TLS encryption. Strongly recommended on public networks."
      ~default:
        (match existing with Some c -> c.Runtime_config.tls | None -> true)
      ()
  in
  let nick_field =
    Setup_tui.make_field ~key:"n" ~label:"Nick" ~menu_label:"Set bot nick"
      ~description:
        "IRC nickname for the bot. No spaces allowed. Register it with \
         NickServ before use."
      ~validate:validate_nick
      ~default:
        (match existing with Some c -> c.Runtime_config.nick | None -> "")
      ()
  in
  let password_field =
    Setup_tui.make_secret_field ~key:"w" ~label:"Password"
      ~menu_label:"Set NickServ password (optional)"
      ~description:
        "NickServ/SASL authentication password. Leave empty if no auth is \
         needed."
      ~default:
        (match existing with
        | Some c -> Option.value ~default:"" c.Runtime_config.password
        | None -> "")
      ()
  in
  let sasl_field =
    Setup_tui.make_bool_field ~key:"a" ~label:"SASL"
      ~menu_label:"Toggle SASL authentication"
      ~description:
        "Enable SASL PLAIN authentication. Recommended over NickServ-only auth."
      ~default:
        (match existing with Some c -> c.Runtime_config.sasl | None -> false)
      ()
  in
  let channels_field =
    Setup_tui.make_list_field ~key:"c" ~label:"Channels"
      ~menu_label:"Set IRC channels to join"
      ~description:
        "Comma-separated channel names to auto-join. Include '#' prefix, e.g. \
         #mychannel,#another"
      ~default:
        (match existing with Some c -> c.Runtime_config.channels | None -> [])
      ()
  in
  let allow_from_field =
    Setup_tui.make_list_field ~key:"f" ~label:"Allow From"
      ~menu_label:"Set allowed nicks"
      ~description:
        "Comma-separated IRC nicks allowed to send commands, or * for all. \
         Example: alice,bob"
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let fields =
    [
      host_field;
      port_field;
      tls_field;
      nick_field;
      password_field;
      sasl_field;
      channels_field;
      allow_from_field;
    ]
  in
  let build_json () =
    build_irc_json
      ~host:(Setup_tui.get_str host_field)
      ~port:(Setup_tui.get_int port_field)
      ~tls:(Setup_tui.get_bool tls_field)
      ~nick:(Setup_tui.get_str nick_field)
      ~password:(Setup_tui.get_str password_field)
      ~sasl:(Setup_tui.get_bool sasl_field)
      ~channels:(Setup_tui.get_str_list channels_field)
      ~allow_from:(Setup_tui.get_str_list allow_from_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str host_field = "" then Error "Host is required."
    else if Setup_tui.get_str nick_field = "" then Error "Nick is required."
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "IRC Channel Configuration";
      docs_url = "https://clawq.org/channels/#irc";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
