(* setup_discord.ml — Interactive setup wizard for Discord bot integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_bot_token s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Bot token cannot be empty." else Ok trimmed

let default_intents = 33281

(* GUILDS=1, GUILD_MESSAGES=512, MESSAGE_CONTENT=32768 *)
let intent_flags =
  [
    ("GUILDS", 1);
    ("GUILD_MEMBERS", 2);
    ("GUILD_MODERATION", 4);
    ("GUILD_EXPRESSIONS", 8);
    ("GUILD_INTEGRATIONS", 16);
    ("GUILD_WEBHOOKS", 32);
    ("GUILD_INVITES", 64);
    ("GUILD_VOICE_STATES", 128);
    ("GUILD_PRESENCES", 256);
    ("GUILD_MESSAGES", 512);
    ("GUILD_MESSAGE_REACTIONS", 1024);
    ("GUILD_MESSAGE_TYPING", 2048);
    ("DIRECT_MESSAGES", 4096);
    ("DIRECT_MESSAGE_REACTIONS", 8192);
    ("DIRECT_MESSAGE_TYPING", 16384);
    ("MESSAGE_CONTENT", 32768);
    ("GUILD_SCHEDULED_EVENTS", 65536);
    ("AUTO_MODERATION_CONFIGURATION", 1048576);
    ("AUTO_MODERATION_EXECUTION", 2097152);
  ]

let intent_names intents =
  List.filter_map
    (fun (name, bit) -> if intents land bit <> 0 then Some name else None)
    intent_flags

let build_discord_json ~bot_token ~allow_guilds ~allow_users ~intents =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "discord",
              `Assoc
                [
                  ("bot_token", `String bot_token);
                  ( "allow_guilds",
                    `List (List.map (fun s -> `String s) allow_guilds) );
                  ( "allow_users",
                    `List (List.map (fun s -> `String s) allow_users) );
                  ("intents", `Int intents);
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete Discord bot setup:

    1. Go to: https://discord.com/developers/applications
    2. Click "New Application", give it a name, and create
    3. Go to "Bot" in the left sidebar
    4. Copy the bot token and paste it into this wizard
    5. Under "Privileged Gateway Intents", enable:
       - MESSAGE CONTENT INTENT (required)
       - SERVER MEMBERS INTENT (optional, for member info)
    6. Go to "OAuth2" > "URL Generator"
    7. Select scopes: bot
    8. Select bot permissions: Send Messages, Read Message History
    9. Copy the generated URL and open it to invite the bot to your server

  Invite URL pattern:
    https://discord.com/oauth2/authorize?client_id=YOUR_APP_ID&permissions=68608&scope=bot

  Note: Replace YOUR_APP_ID with your application's Client ID
  from the "General Information" page.

  Full documentation: https://clawq.org/channels/#discord
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.discord
  with _ -> None

(* ── Intent toggler ──────────────────────────────────────────────── *)

let prompt_toggle_intents ~intents_field =
  let open Setup_common in
  let current_intents = Setup_tui.get_int intents_field in
  Printf.printf "\n";
  Printf.printf "  %s\n\n" (bold "Toggle Gateway Intents");
  Printf.printf "  %s\n\n"
    (dim "Current intents shown with [x], disabled with [ ]");
  List.iteri
    (fun i (name, bit) ->
      let enabled = current_intents land bit <> 0 in
      let marker = if enabled then green "[x]" else dim "[ ]" in
      Printf.printf "    %s  %s  %s\n"
        (cyan (Printf.sprintf "%2d" (i + 1)))
        marker name)
    intent_flags;
  Printf.printf "\n";
  let p =
    Printf.sprintf "  %s Toggle by number (or Enter to finish): " (cyan ">")
  in
  let line = String.trim (Tui_input.read_line_clean p) in
  match int_of_string_opt line with
  | Some idx when idx >= 1 && idx <= List.length intent_flags ->
      let _, bit = List.nth intent_flags (idx - 1) in
      intents_field.value := string_of_int (current_intents lxor bit)
  | _ -> ()

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let bot_token =
    Setup_tui.make_secret_field ~key:"t" ~label:"Bot Token"
      ~menu_label:"Set bot token"
      ~description:
        "Discord bot token. Get it from: \
         https://discord.com/developers/applications > Bot > Copy token"
      ~validate:validate_bot_token
      ~default:
        (match existing with
        | Some d -> d.Runtime_config.bot_token
        | None -> "")
      ()
  in
  let allow_guilds =
    Setup_tui.make_list_field ~key:"g" ~label:"Allow Guilds"
      ~menu_label:"Set allowed guilds"
      ~description:"Comma-separated guild/server IDs, or * for all."
      ~default:
        (match existing with
        | Some d -> d.Runtime_config.allow_guilds
        | None -> [ "*" ])
      ()
  in
  let allow_users =
    Setup_tui.make_list_field ~key:"u" ~label:"Allow Users"
      ~menu_label:"Set allowed users"
      ~description:"Comma-separated user IDs, or * for all."
      ~default:
        (match existing with
        | Some d -> d.Runtime_config.allow_users
        | None -> [ "*" ])
      ()
  in
  let intents_field =
    Setup_tui.make_int_field ~key:"n" ~label:"Intents (raw)"
      ~menu_label:"Set intents value"
      ~description:
        "Gateway intents bitmask. Use the toggle action for easier editing."
      ~default:
        (match existing with
        | Some d -> d.Runtime_config.intents
        | None -> default_intents)
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = "Discord Bot Configuration";
      docs_url = "https://clawq.org/channels/#discord";
      fields = [ bot_token; allow_guilds; allow_users; intents_field ];
      extra_actions =
        [
          ( "i",
            "Toggle individual intents",
            fun () -> prompt_toggle_intents ~intents_field );
        ];
      build_json =
        (fun () ->
          build_discord_json
            ~bot_token:(Setup_tui.get_str bot_token)
            ~allow_guilds:(Setup_tui.get_str_list allow_guilds)
            ~allow_users:(Setup_tui.get_str_list allow_users)
            ~intents:(Setup_tui.get_int intents_field));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str bot_token = "" then
            Error "Bot token is required before saving."
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
