# Slash Command Rendering Compatibility

Internal reference for connector-safe slash-command output.

Last reviewed: 2026-03-17.

## Why This Exists

- `/help` previously rendered as a Markdown table for Teams, which was removed under the incorrect assumption that Teams doesn't support markdown tables.
- That was restored in B435 after confirming Bot Framework v3 text messages DO support markdown tables.
- Slash-command replies should prefer a shared safe text layout, with connector-specific rich rendering only when the connector has confirmed support.

## Connector Summary

### Teams

Source: https://learn.microsoft.com/en-us/microsoftteams/platform/resources/bot-v3/bots-text-formats

- Bot Framework v3 text messages support markdown tables when `textFormat` is `"markdown"` (default).
- Markdown tables render natively on desktop, web, iOS, and Android Teams clients.
- Teams uses markdown tables for tabular data (`/costs`, `/usage`, `/status`, `/help`, `/tools`, `/model usage`).
- A blank line (`\n\n`) is required before pipe tables for Teams' markdown renderer to recognize them as table blocks.

### Telegram

Sources:
- https://core.telegram.org/bots/api#formatting-options
- https://core.telegram.org/bots/api#html-style
- https://core.telegram.org/bots/api#markdownv2-style

- Supports a limited HTML/MarkdownV2 formatting set.
- Supported HTML tags include `b`, `i`, `u`, `s`, `code`, `pre`, `a`, `blockquote`, `blockquote expandable`, spoiler tags, and Telegram-specific emoji/time tags.
- Only documented tags are supported; tables are not supported.
- Safe rich mode for slash commands: Telegram HTML.

### Slack

Source: https://docs.slack.dev/messaging/formatting-message-text/

- Top-level message text uses `mrkdwn`.
- Supported text features include bold, italic, strike, quotes, inline code, fenced code blocks, links, and manual list-like text.
- Rich layouts belong in Block Kit, not Markdown tables.
- Safe default for slash commands: plain line-oriented text or simple `mrkdwn` emphasis; do not rely on tables.

### Discord

Source: https://support.discord.com/hc/en-us/articles/210298617-Markdown-Text-101-Chat-Formatting-Bold-Italic-Underline

- Supports common Markdown features such as emphasis, headers, lists, code blocks, and block quotes.
- No documented table support in regular message formatting.
- Safe default for slash commands: plain line-oriented text or simple Markdown; do not rely on tables.

### Web / HTTP / Plain Text

- Treat as plain text unless a caller explicitly renders richer content.
- Safe default for slash commands: plain line-oriented text.

## Repo Policy

- Default slash-command layout should be connector-safe plain text.
- Connector-specific rich renderers are allowed when they are explicitly supported and already wired for that connector.
- Telegram uses dedicated rich HTML rendering. Discord and Slack use code blocks for tabular data. Teams uses native markdown tables.
- New slash-command output should be modeled as structured sections/rows first, then rendered per connector.
- Do not introduce Markdown tables for connectors other than Teams.

## Practical Rules

- Use aligned lines, short headings, bullet lists, and code formatting sparingly.
- Avoid Markdown tables on Discord, Slack, and Telegram. Teams supports them natively.
- For tabular data: use `Format_adapter.render_table` which dispatches per connector — Teams gets `Table_format.render_markdown`, Telegram gets `<pre>`-wrapped plaintext, Plain gets raw plaintext, and others get `code_block`-wrapped plaintext.
- If interactive UI is needed, prefer `Rich_message` with a text fallback.
- When adding a new slash command, decide whether it needs:
  - a shared plain renderer only, or
  - a shared plain renderer plus connector-specific variants (Telegram HTML, Teams markdown tables).

## Current Implementation Direction

- `src/slash_commands.ml` owns slash-command content rendering.
- Connectors select a rendering target instead of formatting replies ad hoc.
- Telegram uses HTML-specific renderers (`<pre>` for tables, `<b>` for headings).
- Teams uses native markdown tables (`Table_format.render_markdown`) for tabular output like `/costs`, `/usage`, `/model usage`, `/status`, `/tools`, and `/help`.
- Teams applies `Markdown_util.normalize_tables` to all outbound messages (in `build_reply_body`). This post-processes free-form LLM text to ensure blank lines around tables, insert missing separator rows, and add trailing pipes — fixing common rendering issues with Teams' strict markdown parser. The transform is idempotent, so already well-formed tables pass through unchanged.
- Discord and Slack use `Format_adapter.code_block` (triple-backtick fences) for tabular output.
- Web/Plain receives raw text (no code block wrapping).

## Menu Subcommands

Six slash commands support interactive menu modes with connector-specific rendering:

- `/model menu [page]` — select from favorite models
- `/thinking menu` — select thinking level
- `/config menu [page]` — browse config sections
- `/skills [page]` — list and invoke available skills
- `/costs menu` — select cost view
- `/bg menu` — background task actions

**Teams**: Renders as Adaptive Cards with `imBack` action buttons (9 items per page, prev/next nav buttons for pagination). Generated by `slash_commands_manifest.ml` (`button_card`, `*_menu_adaptive_card_json` functions).

**Other connectors**: Renders as text-formatted menus via `slash_commands_fmt.ml` (`format_*_menu` functions) using each connector's `Format_adapter` for code/bold styling. Pagination footer shows prev/next command links.
