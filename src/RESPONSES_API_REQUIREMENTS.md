# OpenAI Responses API Input Requirements

Reference for `provider_openai_codex.ml` — constraints on the `/v1/responses` input array.

## Input Array Structure

The `input` parameter accepts a flat array of **items** (not nested messages).
Item types include: `message` (with role), `function_call`, `function_call_output`,
`reasoning`, `item_reference`, and various built-in tool call types.

### Message Ordering

- The Responses API uses **items** not chat-style messages. Items are a flat sequence.
- **Consecutive user messages ARE allowed** — no strict alternation required.
- function_call items and function_call_output items should be grouped:
  `[function_call, function_call, function_call_output, function_call_output]`
  (all calls first, then all outputs). This is the "in-distribution" ordering.
- After function_call_output items, if the conversation continues, an **assistant
  message** item should appear before subsequent user messages. The API may reject
  input where user messages follow directly after function_call_outputs with no
  intervening assistant turn.

### Reasoning Models (o3, o4-mini, GPT-5 with reasoning)

- For reasoning models, `function_call` items **require** a preceding `reasoning`
  item. Error: `"function_call was provided without its required reasoning item"`.
- A `reasoning` item must be followed by its associated item (function_call or
  message). Error: `"reasoning was provided without its required following item"`.
- Use `include=["reasoning.encrypted_content"]` to get opaque reasoning that can
  be passed back without exposing chain-of-thought.
- Non-reasoning models (gpt-5.x standard, gpt-5.x-codex) do not require reasoning items.
- If a model starts returning reasoning items, they MUST be stored and replayed.

### Phase Field (gpt-5.3-codex)

- The `phase` field on assistant output items (`"phase": "commentary"` or
  `"phase": "final_answer"`) must be preserved when replaying history.
- Dropping `phase` during history reconstruction causes significant performance
  degradation on gpt-5.3-codex.
- `phase` is only supported on assistant items — do not add to user messages.

### Tool Call Validation (Bidirectional)

| Rule | Error if violated |
|------|-------------------|
| Every `function_call_output` needs matching `function_call` | `"No tool call found for function call output with call_id"` |
| Every `function_call` needs matching `function_call_output` | `"No tool output found for function call"` |
| `function_call` must appear before its output | Implicit from call_id matching |
| For reasoning models: `reasoning` before `function_call` | `"function_call without required reasoning item"` |
| For reasoning models: `reasoning` followed by its item | `"reasoning without required following item"` |

### Tool Response Truncation

- Recommended limit: ~10k tokens per tool response (approximate via `num_bytes / 4`).
- Truncation strategy: 50% beginning, 50% end, with "…N tokens truncated…" in middle.

### Content Encoding

- Text content must be valid UTF-8 with control characters properly JSON-escaped.
- ANSI escape sequences (ESC = 0x1B) in tool outputs must be escaped as `\u001b`.
- Empty or null content where a string is expected may cause 400.

### Instructions Parameter

- System-level content goes in the `instructions` field, not in input items.
- `instructions` take priority over prompt content in `input`.
- When using `previous_response_id`, instructions from previous turns are NOT retained.

## Common 400 Error Causes

1. `function_call` without required `reasoning` item (reasoning models only)
2. `function_call_output` without matching `function_call`
3. `function_call` without matching `function_call_output`
4. Dropped `phase` field on assistant items (gpt-5.3-codex — performance, not hard error)
5. Invalid input item structure or missing required fields
6. Malformed JSON or invalid field types
7. Context window overflow (generic "Bad Request" with no detail)
8. Dangling `reasoning` item without following item

## Sources

- https://developers.openai.com/api/reference/resources/responses/methods/create/
- https://developers.openai.com/api/docs/guides/function-calling/
- https://platform.openai.com/docs/guides/migrate-to-responses
- https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide/
- https://cookbook.openai.com/examples/o-series/o3o4-mini_prompting_guide
- https://developers.openai.com/cookbook/examples/responses_api/reasoning_items
- https://community.openai.com/t/issue-with-new-responses-api-400-no-tool-call-found-for-function-call-output-with-call-id/1142327
- https://community.openai.com/t/responses-api-invalid-request-error-function-call-was-provided-without-its-required-reasoning-item/1236046
- https://community.openai.com/t/openai-api-error-function-call-was-provided-without-its-required-reasoning-item-the-real-issue/1355347
