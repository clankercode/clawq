(** Connector-specific text formatting *)

type connector =
  | Telegram_markdown
  | Telegram_mdv2
  | Telegram_html
  | Discord
  | Slack
  | Plain

let escape connector text =
  match connector with
  | Telegram_html ->
      let buf = Buffer.create (String.length text + 16) in
      String.iter
        (fun c ->
          match c with
          | '&' -> Buffer.add_string buf "&amp;"
          | '<' -> Buffer.add_string buf "&lt;"
          | '>' -> Buffer.add_string buf "&gt;"
          | _ -> Buffer.add_char buf c)
        text;
      Buffer.contents buf
  | Telegram_markdown | Telegram_mdv2 | Discord | Slack | Plain -> text

let of_parse_mode = function
  | "HTML" -> Telegram_html
  | "MarkdownV2" -> Telegram_mdv2
  | "Markdown" -> Discord
  | "mrkdwn" -> Slack
  | _ -> Plain

let parse_mode_string = function
  | Telegram_html -> "HTML"
  | Telegram_mdv2 -> "MarkdownV2"
  | Telegram_markdown -> "Markdown"
  | Discord -> "Markdown"
  | Slack -> "mrkdwn"
  | Plain -> "Markdown"

let bold connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 -> "*" ^ text ^ "*"
  | Telegram_html -> "<b>" ^ text ^ "</b>"
  | Discord -> "**" ^ text ^ "**"
  | Slack -> "*" ^ text ^ "*"
  | Plain -> text

let italic connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Slack -> "_" ^ text ^ "_"
  | Telegram_html -> "<i>" ^ text ^ "</i>"
  | Discord -> "*" ^ text ^ "*"
  | Plain -> text

let code connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord | Slack -> "`" ^ text ^ "`"
  | Telegram_html -> "<code>" ^ text ^ "</code>"
  | Plain -> text

let code_block connector text =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord | Slack ->
      "```\n" ^ text ^ "\n```"
  | Telegram_html -> "<pre>" ^ text ^ "</pre>"
  | Plain -> text

let strikethrough connector text =
  match connector with
  | Telegram_mdv2 | Discord | Slack -> "~" ^ text ^ "~"
  | Telegram_html -> "<s>" ^ text ^ "</s>"
  | Telegram_markdown | Plain -> text

let link connector ~text ~url =
  match connector with
  | Telegram_markdown | Telegram_mdv2 | Discord -> "[" ^ text ^ "](" ^ url ^ ")"
  | Telegram_html -> "<a href=\"" ^ url ^ "\">" ^ text ^ "</a>"
  | Slack -> "<" ^ url ^ "|" ^ text ^ ">"
  | Plain -> text ^ " (" ^ url ^ ")"

let blockquote connector text =
  match connector with
  | Telegram_mdv2 | Telegram_html | Discord | Slack -> "> " ^ text
  | Telegram_markdown | Plain -> text
