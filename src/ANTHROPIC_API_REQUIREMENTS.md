# Anthropic Claude Messages API Input Requirements

Reference for `provider_anthropic.ml` — constraints on the `/v1/messages` endpoint.

## Messages Array Structure

- `messages` is required, array of message objects with `role` and `content`.
- Max 100,000 messages per request. API is stateless — full history every time.
- `content` can be a string (shorthand for `[{"type": "text", "text": "..."}]`) or array of content blocks.

## Role Alternation Rules

- **First message must be `user` role.**
- Messages must alternate between `user` and `assistant` roles.
- **Consecutive same-role messages are auto-merged** (not rejected).
- No `"system"` role in messages — use the top-level `system` parameter.
- Pre-filling: last message can be `assistant` role (deprecated on Opus 4.6, Sonnet 4.6, Sonnet 4.5).

## Tool Use / Tool Result Pairing (Strict)

**Rule A — Every `tool_use` must have a matching `tool_result`:**
- Each `tool_use` block in an assistant message must have a corresponding `tool_result`
  in the **immediately following** user message.
- Error: `"tool_use ids were found without tool_result blocks immediately after"`

**Rule B — Every `tool_result` must reference an existing `tool_use`:**
- Each `tool_result.tool_use_id` must match an `id` from a `tool_use` block in the
  **immediately preceding** assistant message.
- Error: `"unexpected tool_use_id found in tool_result blocks: toolu_XXXX"`

**Rule C — No intervening messages** between tool_use and tool_result.

**Rule D — tool_result ordering in content array:**
- In a user message containing tool_results, `tool_result` blocks must come **first**,
  then `text` blocks after.

**Rule E — Tools must be defined:**
- If messages contain any `tool_use` or `tool_result` blocks, the top-level `tools`
  parameter must be present.

**Rule F — Bidirectional 1:1 pairing** within adjacent message pairs.

## Content Block Rules

- **Text blocks**: Must be non-empty. Error: `"text content blocks must be non-empty"`
- **Assistant messages**: Can contain both `text` and `tool_use` blocks.
- **User messages**: Can contain `text`, `image`, and `tool_result` blocks.
- **`tool_use` blocks**: require `id`, `name`, `input` (JSON object).
- **`tool_result` blocks**: require `tool_use_id`, optional `content`, optional `is_error`.

## System Prompt

- Must use top-level `system` parameter, not a message role.
- Can be string or array of content blocks (text blocks with optional cache control).

## Common 400 Error Causes

| Cause | Error |
|-------|-------|
| Orphaned tool_result | `"unexpected tool_use_id found in tool_result blocks"` |
| Orphaned tool_use | `"tool_use ids were found without tool_result blocks immediately after"` |
| Missing tools param | `"Requests must define tools when including tool_use or tool_result blocks"` |
| Invalid input_schema | `"JSON schema is invalid"` |
| Empty text block | `"text content blocks must be non-empty"` |
| History truncation breaking tool pairs | Various pairing errors |
| First message not user | Validation error |

## Sources

- https://platform.claude.com/docs/en/api/messages
- https://platform.claude.com/docs/en/api/prompt-validation
- https://platform.claude.com/docs/en/agents-and-tools/tool-use/implement-tool-use
- https://docs.anthropic.com/en/api/errors
