type command = { name : string; description : string }
type thinking_action = ShowThinking | SetThinking of string option

type result =
  | Reply of string
  | Reset
  | Thinking of thinking_action
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
    { name = "pair"; description = "Pair with TOTP code: /pair <6-digit-code>" };
    {
      name = "update";
      description = "Pull, rebuild, and gracefully restart clawq";
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
        | "status" -> Reply "Bot is running."
        | "thinking" -> (
            match args with
            | [] -> Thinking ShowThinking
            | [ value ] -> (
                match parse_thinking_level value with
                | Some level -> Thinking (SetThinking level)
                | None -> Reply (invalid_thinking_level_message value))
            | _ -> Reply (thinking_usage ()))
        | "" -> NotACommand
        | _ -> NotACommand)

let reset_message = "Session reset. Send a new message to start fresh."
