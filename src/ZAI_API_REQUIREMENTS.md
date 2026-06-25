# Z.AI (Zhipu AI) API Requirements

Reference for OpenAI-compatible provider path when using `zai` or `zai_coding` providers.

## Endpoints

- General API: `https://api.z.ai/api/paas/v4/chat/completions`
- Coding Plan: `https://api.z.ai/api/coding/paas/v4/chat/completions`
- Auth: Bearer token `Authorization: Bearer <api_key>`
- Content-Type: `application/json`

## Coding Plan Models

Only these models are callable under the coding plan quota:
- `glm-5.2`, `glm-5.1`, `glm-5-turbo`, `glm-5`, `glm-4.7`, `glm-4.6`, `glm-4.5`

Other models (e.g. `glm-4.7-flash`) use the general API endpoint, not the coding one.

## Encoding Requirements (Critical)

**The Z.AI API backend uses Java/Jackson for JSON parsing, which strictly validates UTF-8.**

- All JSON request bodies must contain only valid UTF-8 encoded text.
- Invalid UTF-8 bytes (e.g. Windows-1252 smart quotes like `0x9C`) cause HTTP 400:
  ```
  JSON parse error: Invalid UTF-8 start byte 0x9c
  ```
- The error references the exact byte offset and message index in the request.
- **Fix**: `Provider.sanitize_utf8` replaces invalid bytes with U+FFFD before JSON serialization.

## Message Format

Standard OpenAI-compatible chat completions format:
- Required fields: `model`, `messages`
- Messages array: `[{"role": "system"|"user"|"assistant"|"tool", "content": "..."}]`
- Tool calls follow OpenAI format with `tool_calls` array

## Differences from Standard OpenAI API

- At least one `user` message is required; system/assistant-only conversations are rejected.
- `stop` parameter: maximum 1 item (OpenAI allows up to 4).
- `tool_choice`: only `"auto"` is supported (no `"none"` or `"required"`).
- `finish_reason` may include non-standard values: `"sensitive"` (content policy) and `"network_error"`.
- Response may include `web_search` array and `reasoning_content` in message.
- Thinking mode uses `{"thinking": {"type": "enabled"|"disabled", "clear_thinking": bool}}`.

## Context Windows

| Model | Input | Max Output |
|-------|-------|------------|
| GLM-5, GLM-4.7, GLM-4.6 | 200K | 128K |
| GLM-4.5 series | 131K | 96K |
| GLM-4.6v | 128K | 32K |
| GLM-4.5v | 64K | 16K |

## Error Codes

| HTTP | Business Code | Meaning |
|------|---------------|---------|
| 400  | 1210-1234     | Invalid params, missing model, unsupported method, permission denied |
| 400  | —             | JSON parse error (encoding/format issue in request body) |
| 401  | 1000-1004     | Token invalid, expired, or missing |
| 429  | 1300-1310     | Rate/concurrency/daily/weekly limits exceeded |
| 429  | 1110+         | Account in arrears |
| 500  | —             | Server error, retry |

## Content Policy

- Input and output are checked for "unsafe or sensitive content" (codes 1300-1301).
- Violations return HTTP 400 or are surfaced via `finish_reason` in streaming responses.

## Validation Rules

- `content` and `tool_calls` cannot both be empty in a message (error 1214).
- `tools[].type` must not be empty when tools are provided (error 1214).
- `messages` array must be properly structured (error 1214: "The messages parameter is illegal").

## Rate Limits

- Concurrency limits per API key.
- Frequency caps (requests per time window).
- Daily and weekly quotas.
- No explicit per-request token limit documented; governed by model context window.

## Streaming

- Standard SSE format with `data: ` prefix lines.
- `data: [DONE]` signals stream end.
- Streaming errors appear in `finish_reason` rather than standard error codes.
