type command = { name : string; description : string }
type thinking_action = ShowThinking | SetThinking of string option
type show_thinking_action = ShowThinkingStatus | ToggleShowThinking

type result =
  | Reply of string
  | Reset
  | Compact
  | Thinking of thinking_action
  | ShowThinking of show_thinking_action
  | Delegate of string
  | ForkAnd of string
  | NotACommand

let allowed_thinking_levels = [ "low"; "medium"; "high"; "off"; "xhigh"; "max" ]
let thinking_level_to_string = function Some level -> level | None -> "off"

let parse_thinking_level value =
  match String.lowercase_ascii value with
  | "low" -> Some (Some "low")
  | "medium" -> Some (Some "medium")
  | "high" -> Some (Some "high")
  | "off" -> Some None
  | "xhigh" -> Some (Some "xhigh")
  | "max" -> Some (Some "max")
  | _ -> None

let thinking_usage () =
  Printf.sprintf "Usage: /thinking [%s]"
    (String.concat "|" allowed_thinking_levels)

let invalid_thinking_level_message value =
  Printf.sprintf "Invalid thinking level '%s'. Use one of: %s" value
    (String.concat ", " allowed_thinking_levels)

let commands =
  [
    { name = "start"; description = "Start the bot" };
    { name = "help"; description = "Show available commands" };
    { name = "new"; description = "Start a new conversation" };
    { name = "status"; description = "Show bot status" };
    {
      name = "thinking";
      description = "Show or set thinking level: /thinking [level]";
    };
    {
      name = "compact";
      description = "Compact session history (summarize older messages)";
    };
    { name = "pair"; description = "Pair with TOTP code: /pair <6-digit-code>" };
    {
      name = "update";
      description = "Pull, rebuild, and gracefully restart clawq";
    };
    {
      name = "delegate";
      description =
        "Delegate a prompt to a temporary subagent: /delegate <prompt>";
    };
    {
      name = "show_thinking";
      description = "Toggle display of model thinking in responses";
    };
    {
      name = "config";
      description = "View or modify config: /config <show|get|set|keys>";
    };
    {
      name = "fork_and";
      description =
        "Fork the current session and run a prompt: /fork_and <prompt>";
    };
  ]

let help_text =
  let lines =
    List.map (fun c -> Printf.sprintf "/%s - %s" c.name c.description) commands
  in
  "Available commands:\n" ^ String.concat "\n" lines
  ^ "\n\n\
     Prefix a message with ! to interrupt the current turn in this session and \
     send the rest as a normal message."

let handle text =
  let trimmed = String.trim text in
  if String.length trimmed = 0 || trimmed.[0] <> '/' then NotACommand
  else
    let parts =
      String.split_on_char ' ' trimmed |> List.filter (fun part -> part <> "")
    in
    match parts with
    | [] -> NotACommand
    | first :: args -> (
        let cmd =
          if String.length first <= 1 then ""
          else String.sub first 1 (String.length first - 1)
        in
        let cmd_lower = String.lowercase_ascii cmd in
        match cmd_lower with
        | "start" ->
            Reply
              "clawq bot ready. Send me a message and I'll respond using AI.\n\
               Use /help to see available commands. Prefix a message with ! to \
               interrupt the current turn."
        | "help" -> Reply help_text
        | "new" -> Reset
        | "compact" -> Compact
        | "status" -> Reply "Bot is running."
        | "thinking" -> (
            match args with
            | [] -> Thinking ShowThinking
            | [ value ] -> (
                match parse_thinking_level value with
                | Some level -> Thinking (SetThinking level)
                | None -> Reply (invalid_thinking_level_message value))
            | _ -> Reply (thinking_usage ()))
        | "show-thinking" | "show_thinking" | "toggle-show-thinking" -> (
            match args with
            | [] -> ShowThinking ToggleShowThinking
            | [ s ] when String.lowercase_ascii s = "status" ->
                ShowThinking ShowThinkingStatus
            | _ -> Reply "Usage: /show_thinking [status]")
        | "delegate" -> (
            match args with
            | [] -> Reply "Usage: /delegate <prompt>"
            | _ -> Delegate (String.concat " " args))
        | "config" -> (
            match args with
            | [] | [ "help" ] ->
                Reply
                  "Usage: /config <subcommand>\n\n\
                   Subcommands:\n\
                  \  show [SECTION]  — Show config (or a specific section)\n\
                  \  get KEY         — Get a config value by dot-path\n\
                  \  set KEY VALUE   — Set a config value\n\
                  \  keys [PREFIX]   — List valid config key paths\n\
                  \  wizard          — Run the interactive setup wizard"
            | [ "show" ] ->
                let output = Config_show.show None in
                if String.length output > 1500 then
                  let sections =
                    match
                      try Some (Yojson.Safe.from_string output) with _ -> None
                    with
                    | Some (`Assoc fields) ->
                        List.map fst fields |> String.concat ", "
                    | _ -> "(unable to list sections)"
                  in
                  Reply
                    (Printf.sprintf
                       "Config is too large to display in chat. Available \
                        sections:\n\
                        %s\n\n\
                        Use: /config show <section>\n\
                        Example: /config show gateway"
                       sections)
                else Reply output
            | [ "show"; section ] -> Reply (Config_show.show (Some section))
            | [ "get" ] -> Reply "Usage: /config get KEY"
            | [ "get"; key ] -> Reply (Config_set.get_value_redacted key)
            | "set" :: key :: value_parts when value_parts <> [] ->
                let segments = Config_set.split_path key in
                if
                  segments <> [ "" ]
                  && not
                       (Config_set.validate_path segments
                          Config_set.config_schema)
                then Reply (Config_set.suggest_key key segments)
                else if Config_set.is_secret_path key then
                  Reply
                    (Printf.sprintf
                       "Cannot set secret key '%s' via chat. Use the terminal: \
                        clawq config set %s <value>"
                       key key)
                else
                  let value = String.concat " " value_parts in
                  Reply
                    (Config_set.set_value key value
                    ^ "\nNote: restart the daemon for changes to take effect.")
            | [ "set" ] | [ "set"; _ ] -> Reply "Usage: /config set KEY VALUE"
            | [ "keys" ] ->
                let paths = Config_set.config_leaf_paths () in
                Reply (String.concat "\n" paths)
            | [ "keys"; prefix ] ->
                let paths = Config_set.config_leaf_paths () in
                let prefix_lower = String.lowercase_ascii prefix in
                let matches =
                  List.filter
                    (fun p ->
                      let p_lower = String.lowercase_ascii p in
                      String.length p_lower >= String.length prefix_lower
                      && String.sub p_lower 0 (String.length prefix_lower)
                         = prefix_lower)
                    paths
                in
                if matches = [] then
                  Reply
                    (Printf.sprintf "No config keys matching prefix '%s'."
                       prefix)
                else Reply (String.concat "\n" matches)
            | [ "wizard" ] ->
                Reply
                  "The config wizard requires an interactive terminal.\n\
                   Run: clawq config wizard"
            | sub :: _ ->
                Reply
                  (Printf.sprintf
                     "Unknown config subcommand '%s'.\n\
                      Use /config for usage help."
                     sub))
        | "fork-and" | "fork_and" -> (
            match args with
            | [] -> Reply "Usage: /fork_and <prompt>"
            | _ -> ForkAnd (String.concat " " args))
        | "" -> NotACommand
        | _ -> NotACommand)

let reset_message = "Session reset. Send a new message to start fresh."
