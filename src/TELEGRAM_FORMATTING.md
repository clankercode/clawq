# Telegram Formatting Reference (Internal)

## HTML Mode (`parse_mode: "HTML"`)

### Supported Tags

| Tag | Purpose | Example |
|-----|---------|---------|
| `<b>text</b>` | Bold | **text** |
| `<i>text</i>` | Italic | *text* |
| `<u>text</u>` | Underline | |
| `<s>text</s>` | Strikethrough | ~~text~~ |
| `<code>text</code>` | Inline code | `text` |
| `<pre>block</pre>` | Code block | |
| `<pre><code class="language-python">...</code></pre>` | Syntax-highlighted code block | |
| `<a href="URL">text</a>` | Hyperlink | |
| `<blockquote>text</blockquote>` | Block quote | |
| `<blockquote expandable>text</blockquote>` | Collapsible block quote | |
| `<tg-spoiler>text</tg-spoiler>` | Spoiler | |
| `<tg-emoji emoji-id="ID">emoji</tg-emoji>` | Custom emoji | |

### Entity Escaping

Outside of tags, these characters must be escaped:

| Character | Escape |
|-----------|--------|
| `<` | `&lt;` |
| `>` | `&gt;` |
| `&` | `&amp;` |

Inside `<code>` and `<pre>` blocks, only `<`, `>`, and `&` need escaping.

### Message Limits

- Maximum message length: **4096 characters** (UTF-16 code units)
- `chunk_text` in `telegram.ml` splits at this boundary, preferring newline breaks

## MarkdownV2 Mode (`parse_mode: "MarkdownV2"`)

### Syntax

| Format | Syntax |
|--------|--------|
| Bold | `*text*` |
| Italic | `_text_` |
| Underline | `__text__` |
| Strikethrough | `~text~` |
| Spoiler | `\|\|text\|\|` |
| Inline code | `` `text` `` |
| Code block | ` ```lang\ncode\n``` ` |
| Link | `[text](url)` |
| Block quote | `>text` (each line) |
| Expandable quote | `**>text\|\|` (each line) |

### Special Characters (must be escaped with `\`)

```
_ * [ ] ( ) ~ ` > # + - = | { } . !
```

### Where Each Mode is Used in clawq

| Context | Mode | Location |
|---------|------|----------|
| Agent responses | MarkdownV2 | `telegram.ml` main message loop |
| `/tools` command | HTML | `slash_commands.ml:format_tools_telegram` |
| Tool call results | HTML | `status_message.ml` consolidated tool status |
| `/delegate` responses | HTML | `telegram.ml` via `send_chunked_html_with_fallback` |
| `/fork_and` responses | HTML | `telegram.ml` via `send_chunked_html_with_fallback` |
| Status messages | Plain text | Various |

### Expandable Blockquote Patterns

**HTML mode** (preferred for delegate/fork):
```html
<b>Summary:</b> Brief result here.
<blockquote expandable>
Detailed content that can be expanded...
</blockquote>
```

**MarkdownV2 mode**:
```
**>Line 1||
**>Line 2||
```

### Helpers in clawq

- `telegram_format.ml` — MarkdownV2 escaping and formatting utilities
- `telegram.ml:chunk_text` — Splits text at 4096-char boundaries
- `telegram.ml:send_chunked` — Sends multi-chunk messages with optional parse_mode
- `telegram.ml:send_chunked_html_with_fallback` — Sends as HTML, falls back to plain text per chunk
- `telegram.ml:telegram_delegate_prompt` — Appends HTML formatting instructions to delegate/fork prompts
