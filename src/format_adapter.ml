(** Connector-specific text formatting *)

type connector = Telegram_markdown | Telegram_mdv2 | Discord | Slack | Plain

let bold connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 -> "*" ^ text ^ "*"
  | Discord -> "**" ^ text ^ "**"
  | Slack -> "*" ^ text ^ "*"
  | Plain -> text

let italic connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Slack -> "_" ^ text ^ "_"
  | Discord -> "*" ^ text ^ "*"
  | Plain -> text

let code connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord | Slack -> "`" ^ text ^ "`"
  | Plain -> text

let code_block connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord | Slack ->
      "```\n" ^ text ^ "\n```"
  | Plain -> text

let strikethrough connector text =
  match connector with
  | Telegram_mdv2 | Discord | Slack -> "~" ^ text ^ "~"
  | Telegram_markdown | Plain -> text

let link connector ~text ~url =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord -> "[" ^ text ^ "](" ^ url ^ ")"
  | Slack -> "<" ^ url ^ "|" ^ text ^ ">"
  | Plain -> text ^ " (" ^ url ^ ")"

let blockquote connector text =
  match connector with
  | Telegram_mdv2 | Discord | Slack -> "> " ^ text
  | Telegram_markdown | Plain -> text
