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
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.discord
  with _ -> None

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~bot_token ~allow_guilds ~allow_users ~intents =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let active_intents = intent_names intents in
  let intents_str =
    if active_intents = [] then dim "(none)"
    else String.concat ", " active_intents
  in
  draw_box ~width:w
    [
      bold " Discord Bot Configuration ";
      "";
      Printf.sprintf "  Bot Token:      %s"
        (if bot_token = "" then dim "(not set)"
         else green (Tui_input.redact bot_token));
      Printf.sprintf "  Allow Guilds:   %s" (String.concat ", " allow_guilds);
      Printf.sprintf "  Allow Users:    %s" (String.concat ", " allow_users);
      Printf.sprintf "  Intents:        %s %s"
        (cyan (string_of_int intents))
        (dim ("(" ^ intents_str ^ ")"));
      "";
    ];
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_discord_config ~bot_token ~allow_guilds ~allow_users ~intents =
  let open Setup_common in
  let json =
    build_discord_json ~bot_token ~allow_guilds ~allow_users ~intents
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

(* ── Intent toggler ──────────────────────────────────────────────── *)

let prompt_toggle_intents ~current_intents =
  let open Setup_common in
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
  Printf.printf "  %s Toggle by number (or Enter to finish): " (cyan ">");
  flush stdout;
  let line = String.trim (input_line stdin) in
  match int_of_string_opt line with
  | Some idx when idx >= 1 && idx <= List.length intent_flags ->
      let _, bit = List.nth intent_flags (idx - 1) in
      current_intents lxor bit
  | _ -> current_intents

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let bot_token =
        ref
          (match existing with
          | Some d -> d.Runtime_config.bot_token
          | None -> "")
      in
      let allow_guilds =
        ref
          (match existing with
          | Some d -> d.Runtime_config.allow_guilds
          | None -> [ "*" ])
      in
      let allow_users =
        ref
          (match existing with
          | Some d -> d.Runtime_config.allow_users
          | None -> [ "*" ])
      in
      let intents =
        ref
          (match existing with
          | Some d -> d.Runtime_config.intents
          | None -> default_intents)
      in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~bot_token:!bot_token ~allow_guilds:!allow_guilds
          ~allow_users:!allow_users ~intents:!intents;
        let options =
          [ ("t", "Set bot token") ]
          @ [ ("g", "Set allowed guilds") ]
          @ [ ("u", "Set allowed users") ]
          @ [ ("i", "Toggle intents") ]
          @ [ ("h", "Show setup instructions") ]
          @
          if !dirty then [ ("s", Setup_common.bold "Save configuration") ]
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
                if !bot_token = "" then (
                  Setup_common.print_warning
                    "Bot token is not set. Set it before saving.";
                  Setup_common.press_enter_to_continue ())
                else (
                  ignore
                    (save_discord_config ~bot_token:!bot_token
                       ~allow_guilds:!allow_guilds ~allow_users:!allow_users
                       ~intents:!intents);
                  quit := true)
              end
              else quit := true
            end
            else quit := true
        | "t" ->
            Printf.printf "\n";
            Printf.printf "  %s\n"
              (Setup_common.dim
                 "Get your bot token from: \
                  https://discord.com/developers/applications");
            Printf.printf "  %s\n\n"
              (Setup_common.dim "Select your application > Bot > Copy token");
            (if !bot_token <> "" then (
               Printf.printf "  Current token: %s\n\n"
                 (Setup_common.green (Tui_input.redact !bot_token));
               let change =
                 Setup_common.prompt_yn ~prompt:"Change bot token?"
                   ~default:false ()
               in
               if change then
                 match Setup_common.prompt_secret ~prompt:"Bot token" () with
                 | Ok tok -> (
                     match validate_bot_token tok with
                     | Ok t ->
                         bot_token := t;
                         dirty := true
                     | Error e -> Setup_common.print_error e)
                 | Error e -> Setup_common.print_error e)
             else
               match Setup_common.prompt_secret ~prompt:"Bot token" () with
               | Ok tok -> (
                   match validate_bot_token tok with
                   | Ok t ->
                       bot_token := t;
                       dirty := true
                   | Error e -> Setup_common.print_error e)
               | Error e -> Setup_common.print_error e);
            Setup_common.press_enter_to_continue ()
        | "g" ->
            Printf.printf "\n";
            Printf.printf "  %s\n"
              (Setup_common.dim "Comma-separated guild/server IDs, or * for all");
            let default = String.concat "," !allow_guilds in
            let input =
              Setup_common.prompt_string ~prompt:"Allowed guilds" ~default ()
            in
            let guilds =
              String.split_on_char ',' input
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            allow_guilds := if guilds = [] then [ "*" ] else guilds;
            dirty := true;
            Setup_common.press_enter_to_continue ()
        | "u" ->
            Printf.printf "\n";
            Printf.printf "  %s\n"
              (Setup_common.dim "Comma-separated user IDs, or * for all");
            let default = String.concat "," !allow_users in
            let input =
              Setup_common.prompt_string ~prompt:"Allowed users" ~default ()
            in
            let users =
              String.split_on_char ',' input
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            allow_users := if users = [] then [ "*" ] else users;
            dirty := true;
            Setup_common.press_enter_to_continue ()
        | "i" ->
            let new_intents = prompt_toggle_intents ~current_intents:!intents in
            if new_intents <> !intents then (
              intents := new_intents;
              dirty := true)
        | "h" ->
            Printf.printf "%s" post_setup_instructions;
            Setup_common.press_enter_to_continue ()
        | "s" when !dirty ->
            if !bot_token = "" then (
              Setup_common.print_warning "Bot token is required before saving.";
              Setup_common.press_enter_to_continue ())
            else (
              if
                save_discord_config ~bot_token:!bot_token
                  ~allow_guilds:!allow_guilds ~allow_users:!allow_users
                  ~intents:!intents
              then dirty := false;
              Setup_common.press_enter_to_continue ())
        | s ->
            Setup_common.print_warning (Printf.sprintf "Unknown option: %s" s);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes."
      else "Discord setup complete."
