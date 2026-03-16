(* setup_matrix.ml — Interactive setup wizard for Matrix integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_homeserver_url s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Homeserver URL cannot be empty. Example: https://matrix.org or \
       https://your-homeserver.example.com"
  else if
    (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://")
    || (String.length trimmed >= 7 && String.sub trimmed 0 7 = "http://")
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Homeserver URL must start with https:// or http://, got: %s. \
          Example: https://matrix.org"
         trimmed)

let validate_access_token s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Access token cannot be empty. Obtain it via:\n\
      \       Element: Settings > Help & About > Access Token\n\
      \       API: POST /_matrix/client/v3/login with your credentials"
  else Ok trimmed

let validate_user_id s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "User ID cannot be empty."
  else
    (* Must match @user:domain format *)
    let len = String.length trimmed in
    if len < 3 then Error "User ID is too short. Expected format: @user:domain."
    else if trimmed.[0] <> '@' then
      Error "User ID must start with '@'. Expected format: @user:domain."
    else
      try
        let colon_pos = String.index trimmed ':' in
        if colon_pos < 2 then
          Error
            "User ID must have a local part before ':'. Format: @user:domain."
        else if colon_pos >= len - 1 then
          Error "User ID must have a domain after ':'. Format: @user:domain."
        else Ok trimmed
      with Not_found ->
        Error "User ID must contain ':'. Expected format: @user:domain."

let build_matrix_json ~homeserver_url ~access_token ~user_id ~allow_rooms
    ~allow_users =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "matrix",
              `Assoc
                [
                  ("homeserver_url", `String homeserver_url);
                  ("access_token", `String access_token);
                  ("user_id", `String user_id);
                  ( "allow_rooms",
                    `List (List.map (fun s -> `String s) allow_rooms) );
                  ( "allow_users",
                    `List (List.map (fun s -> `String s) allow_users) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete Matrix bot setup:

    1. Create a dedicated Matrix account for your bot:
         Register at https://app.element.io or on your own homeserver.
         Username example: @clawqbot:matrix.org

    2. Log in and obtain an access token. Three options:
         a) Element Web: Settings (gear) > Help & About > scroll to "Access Token"
         b) curl:
              curl -X POST 'https://matrix.org/_matrix/client/v3/login' \
                -H 'Content-Type: application/json' \
                -d '{"type":"m.login.password","user":"@bot:matrix.org","password":"YOUR_PW"}'
         c) Any Matrix client that exposes the access token in settings.

    3. Enter your bot's full Matrix user ID, e.g. @clawqbot:matrix.org
       Format: @localpart:homeserver.domain

    4. Set homeserver URL to the base URL of your homeserver:
         https://matrix.org  (for matrix.org accounts)
         https://your-homeserver.example.com  (for self-hosted)

    5. Configure allow_rooms:
         *  Respond in any room the bot is invited to.
         !roomid:homeserver  Restrict to specific rooms (use room ID, not alias).

    6. Configure allow_users:
         *  Respond to any user.
         @alice:matrix.org,@bob:example.com  Restrict to specific users.

    7. Invite the bot to your room:
         /invite @clawqbot:matrix.org

    8. Start the daemon: clawq daemon start

  Common issues:
    - "M_FORBIDDEN": bot not invited to the room, or allow_rooms is too restrictive.
    - "M_UNKNOWN_TOKEN": access token is expired or invalid; regenerate it.
    - Messages not received: check that allow_users includes your user ID.

  Full documentation: https://clawq.org/channels/#matrix
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.matrix
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let homeserver_url_field =
    Setup_tui.make_field ~key:"u" ~label:"Homeserver URL"
      ~menu_label:"Set homeserver URL"
      ~description:"Matrix homeserver URL, e.g. https://matrix.org"
      ~validate:validate_homeserver_url
      ~default:
        (match existing with
        | Some m -> m.Runtime_config.homeserver_url
        | None -> "")
      ()
  in
  let access_token_field =
    Setup_tui.make_secret_field ~key:"t" ~label:"Access Token"
      ~menu_label:"Set access token"
      ~description:
        "Matrix access token for the bot account. Obtain via Element: Settings \
         > Help & About > Access Token"
      ~validate:validate_access_token
      ~default:
        (match existing with
        | Some m -> m.Runtime_config.access_token
        | None -> "")
      ()
  in
  let user_id_field =
    Setup_tui.make_field ~key:"i" ~label:"User ID" ~menu_label:"Set user ID"
      ~description:
        "Bot's full Matrix user ID in @localpart:homeserver format, e.g. \
         @clawqbot:matrix.org"
      ~validate:validate_user_id
      ~default:
        (match existing with Some m -> m.Runtime_config.user_id | None -> "")
      ()
  in
  let allow_rooms_field =
    Setup_tui.make_list_field ~key:"r" ~label:"Allow Rooms"
      ~menu_label:"Set allowed rooms"
      ~description:
        "Comma-separated Matrix room IDs, or * for all. Room IDs start with \
         '!', e.g. !roomid:matrix.org"
      ~default:
        (match existing with
        | Some m -> m.Runtime_config.allow_rooms
        | None -> [ "*" ])
      ()
  in
  let allow_users_field =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow Users"
      ~menu_label:"Set allowed users"
      ~description:
        "Comma-separated Matrix user IDs allowed to send commands, or * for \
         all. Example: @alice:matrix.org,@bob:example.com"
      ~default:
        (match existing with
        | Some m -> m.Runtime_config.allow_users
        | None -> [ "*" ])
      ()
  in
  let fields =
    [
      homeserver_url_field;
      access_token_field;
      user_id_field;
      allow_rooms_field;
      allow_users_field;
    ]
  in
  let build_json () =
    build_matrix_json
      ~homeserver_url:(Setup_tui.get_str homeserver_url_field)
      ~access_token:(Setup_tui.get_str access_token_field)
      ~user_id:(Setup_tui.get_str user_id_field)
      ~allow_rooms:(Setup_tui.get_str_list allow_rooms_field)
      ~allow_users:(Setup_tui.get_str_list allow_users_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str homeserver_url_field = "" then
      Error "Homeserver URL is required. Example: https://matrix.org"
    else if Setup_tui.get_str access_token_field = "" then
      Error
        "Access token is required. Obtain it from Element: Settings > Help & \
         About > Access Token"
    else if Setup_tui.get_str user_id_field = "" then
      Error
        "User ID is required. Format: @localpart:homeserver, e.g. \
         @clawqbot:matrix.org"
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "Matrix Channel Configuration";
      docs_url = "https://clawq.org/channels/#matrix";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
