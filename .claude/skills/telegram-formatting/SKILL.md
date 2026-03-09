# Telegram Formatting Skill

Use this skill when writing or modifying code that produces Telegram-formatted messages in clawq.

## HTML Mode (preferred for structured output)

### Tags
- `<b>bold</b>`, `<i>italic</i>`, `<u>underline</u>`, `<s>strikethrough</s>`
- `<code>inline code</code>`, `<pre>code block</pre>`
- `<pre><code class="language-python">highlighted</code></pre>`
- `<a href="URL">link text</a>`
- `<blockquote>quote</blockquote>`, `<blockquote expandable>collapsible</blockquote>`
- `<tg-spoiler>hidden</tg-spoiler>`

### Escaping (outside tags)
- `<` → `&lt;`, `>` → `&gt;`, `&` → `&amp;`

## MarkdownV2 Mode (used for agent responses)

### Syntax
- Bold: `*text*`, Italic: `_text_`, Code: `` `text` ``
- Code block: ` ```lang\n...\n``` `
- Quote: `>line` (per line), Expandable: `**>line||` (per line)

### Must-escape characters
```
_ * [ ] ( ) ~ ` > # + - = | { } . !
```

## Expandable Blockquote Pattern

Best practice for fitting large content into a single message:

```html
<b>Result:</b> Brief summary here.
<blockquote expandable>
1. Step one details
2. Step two details
3. Step three details
</blockquote>
```

## clawq Conventions

- Max message length: 4096 chars (split via `chunk_text` in `telegram.ml`)
- `send_chunked` — sends with optional `?parse_mode`, splits automatically
- `send_chunked_html_with_fallback` — sends as HTML, falls back to plain text per chunk on error
- `telegram_delegate_prompt` — wraps user prompt with HTML formatting instructions for delegate/fork
- `telegram_format.ml` — MarkdownV2 escaping helpers (`escape_mdv2`, `bold`, `italic`, `code_block`, etc.)
- `/tools` uses HTML mode with `<blockquote expandable>` for tool listings
- Agent chat responses use MarkdownV2 via `telegram_format.ml`
- Delegate/fork responses use HTML with fallback
