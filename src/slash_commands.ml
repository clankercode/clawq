type command = { name : string; description : string }
type result = Reply of string | Reset | NotACommand

let commands =
  [
    { name = "start"; description = "Start the bot" };
    { name = "help"; description = "Show available commands" };
    { name = "new"; description = "Start a new conversation" };
    { name = "status"; description = "Show bot status" };
    { name = "pair"; description = "Pair with TOTP code: /pair <6-digit-code>" };
  ]

let help_text =
  let lines =
    List.map (fun c -> Printf.sprintf "/%s - %s" c.name c.description) commands
  in
  "Available commands:\n" ^ String.concat "\n" lines

let handle text =
  let trimmed = String.trim text in
  if String.length trimmed = 0 || trimmed.[0] <> '/' then NotACommand
  else
    let cmd =
      match String.index_opt trimmed ' ' with
      | Some i -> String.sub trimmed 1 (i - 1)
      | None -> String.sub trimmed 1 (String.length trimmed - 1)
    in
    let cmd_lower = String.lowercase_ascii cmd in
    match cmd_lower with
    | "start" ->
        Reply
          "clawq bot ready. Send me a message and I'll respond using AI.\n\
           Use /help to see available commands."
    | "help" -> Reply help_text
    | "new" -> Reset
    | "status" -> Reply "Bot is running."
    | "" -> NotACommand
    | _ -> NotACommand

let reset_message = "Session reset. Send a new message to start fresh."
