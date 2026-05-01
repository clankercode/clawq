# MiniMax Provider Implementation Plan

## Overview

Add MiniMax as a first-class provider using their Anthropic-compatible API endpoint. MiniMax supports thinking/reasoning blocks and function calling through this interface.

**Default model**: `minimax:minimax-m2.7-highspeed` with fallback to `minimax:minimax-m2.7`

---

## MiniMax API Summary

### Endpoint
- **Base URL**: `https://api.minimax.io`
- **Anthropic-compatible endpoint**: `/anthropic/v1/messages`
- **Authentication**: `x-api-key` header + `anthropic-version: 2023-06-01`

### Supported Models

| Model | Context Window | Thinking | Input $/M | Output $/M | Cache $/M |
|-------|---------------|----------|-----------|------------|----------|
| MiniMax-M2.7 | 204,800 | ✅ | $0.30 | $1.20 | $0.06 |
| MiniMax-M2.7-highspeed | 204,800 | ✅ | $0.60 | $2.40 | $0.06 |

### Capabilities
- Thinking/reasoning via `type: "thinking"` content blocks
- Streaming with `thinking_delta` + `text_delta` events
- Function calling via `tool_use` blocks
- Temperature, top_p, max_tokens parameters

---

## Files to Create

### 1. `src/provider_minimax.ml`

Native provider implementation using Anthropic-compatible API.

**Key functions**:
- `messages_to_anthropic_json`: Convert `Provider.message` → Anthropic format
  - System/developer messages → extracted separately
  - Tool results → `tool_result` content blocks
  - Assistant tool calls → `tool_use` blocks
- `tools_to_anthropic_json`: Convert OpenAI tools format → Anthropic format
- `parse_response`: Parse non-streaming response
  - Handle `text`, `thinking`, `tool_use` blocks
  - Return `Provider.Text` or `Provider.ToolCalls`
- `complete`: Non-streaming completion
- `complete_streaming`: Streaming with SSE parsing
  - Events: `message_start`, `content_block_start`, `content_block_delta`, `content_block_stop`, `message_delta`, `message_stop`

**Streaming event handling** (matching Anthropic format):
```
event: content_block_delta
data: {"delta":{"type":"thinking_delta","thinking":"..."}}

event: content_block_delta
data: {"delta":{"type":"text_delta","text":"..."}}

event: content_block_delta
data: {"delta":{"type":"input_json_delta","partial_json":"..."}}
```

---

## Files to Modify

### 2. `src/provider.ml`

**Line ~418** - Add `MiniMax` to `provider_kind` type:
```ocaml
type provider_kind =
  | OpenAICompat
  | OpenAICodex
  | Anthropic
  | Ollama
  | Gemini
  | Vertex
  | Cohere
  | MiniMax  (* NEW *)
```

**Line ~439-461** - Add detection in `detect_kind`:
```ocaml
| Some "anthropic" -> Anthropic
| Some "minimax" -> MiniMax  (* NEW *)
```

**Line ~493-504** - Add to `default_base_url_for`:
```ocaml
| "minimax" -> "https://api.minimax.io"
```

### 3. `src/provider_init.ml`

Register MiniMax handlers:
```ocaml
Provider.register_native_complete Provider.MiniMax Provider_minimax.complete;
Provider.register_native_stream Provider.MiniMax Provider_minimax.complete_streaming;
```

### 4. `src/models_catalog.ml`

Add model entries (~lines 625-655):
```ocaml
(* MiniMax *)
{
  provider = "minimax";
  id = "minimax-m2.7";
  display_name = Some "MiniMax-M2.7";
  context_window = Some 204800;
  supports_vision = false;
  supports_tools = true;
  supports_thinking = true;
  deprecated = false;
};
{
  provider = "minimax";
  id = "minimax-m2.7-highspeed";
  display_name = Some "MiniMax-M2.7-highspeed";
  context_window = Some 204800;
  supports_vision = false;
  supports_tools = true;
  supports_thinking = true;
  deprecated = false;
};
```

### 5. `src/cost_tracker.ml`

Add pricing (~lines 192-198):
```ocaml
(* MiniMax *)
( "minimax-m2.7",
  { input_per_m = 0.30; output_per_m = 1.20; cache_read_per_m = Some 0.06 } );
( "minimax-m2.7-highspeed",
  { input_per_m = 0.60; output_per_m = 2.40; cache_read_per_m = Some 0.06 } );
```

### 6. `docs/models.csv`

Add entries (~lines 72-74):
```
minimax,MiniMax-M2.7,"MiniMax M2.7",204800,false,true,"thinking,tools",0.30,1.20,https://platform.minimax.io/docs/pricing/overview,"Flagship",2026-04-30
minimax,MiniMax-M2.7-highspeed,"MiniMax M2.7-highspeed",204800,false,true,"thinking,tools",0.60,2.40,https://platform.minimax.io/docs/pricing/overview,"Highspeed variant",2026-04-30
```

---

## Test Plan

### Unit Tests: `test/test_provider_minimax.ml` (NEW)

Following the pattern of `test/test_provider_anthropic.ml`:

#### Message Conversion Tests
- `test_user_message`: Basic user message conversion
- `test_system_message_filtered`: System message extraction
- `test_tool_result_becomes_user`: Tool result → `tool_result` block
- `test_tool_result_has_tool_use_id`: Tool result includes `tool_use_id`
- `test_assistant_with_tool_calls`: Assistant tool calls → `tool_use` blocks
- `test_mixed_messages`: Multiple message types in sequence

#### System Prompt Extraction Tests
- `test_extract_system_present`: Extract system from messages
- `test_extract_system_absent`: No system returns empty
- `test_extract_system_multiple`: Multiple system messages concatenated

#### Tools Conversion Tests
- `test_tools_none`: None → None
- `test_tools_empty_list`: Empty list → None
- `test_tools_valid`: OpenAI tool → Anthropic format

#### Response Parsing Tests
- `test_parse_text_response`: Basic text response
- `test_parse_thinking_response`: Response with thinking block
- `test_parse_tool_use_response`: Tool call response
- `test_parse_invalid_json`: Error handling
- `test_parse_empty_content`: Empty content handling
- `test_parse_streaming_thinking_and_text`: Both thinking and text in one response

### Integration Tests: `test/test_provider_minimax.ml`

Add integration test cases that make real API calls (tagged `Slow`):

#### `test_provider_minimax_integration.ml` (NEW)

Or add to existing provider integration test suite.

**Setup**:
```ocaml
let minimax_api_key =
  try Some (Sys.getenv "MINIMAX_API_KEY") with Not_found -> None

let require_minimax_key () =
  match minimax_api_key with
  | Some _ -> ()
  | None -> Alcotest.skip "MINIMAX_API_KEY not set"
```

#### Integration Test Cases

**Tier 1: Basic Connectivity**
- `test_minimax_health`: GET request to verify API key is valid
- `test_minimax_models_list`: List available models

**Tier 2: Completion Tests** (require API key, tagged `Slow`)
- `test_simple_completion`: Single user message, no tools
- `test_completion_with_system`: System prompt + user message
- `test_completion_with_thinking`: Verify thinking block is returned
- `test_completion_with_tools`: Request with tools, verify `tool_use` response
- `test_completion_multiturn`: Send history, verify context maintained

**Tier 3: Streaming Tests** (tagged `Slow`)
- `test_streaming_text`: Verify text delta events
- `test_streaming_thinking`: Verify thinking_delta events
- `test_streaming_tool_calls`: Verify tool call deltas
- `test_streaming_combined`: Thinking + text + tool calls interleaved

**Tier 4: Error Handling**
- `test_invalid_api_key`: Verify proper error on bad key
- `test_rate_limit`: Verify rate limit handling
- `test_context_length`: Send exceeding context, verify error

### Test Execution

```bash
# Run unit tests only (fast)
make test-run ARGS="test provider_minimax"

# Run integration tests (requires MINIMAX_API_KEY)
make test-run ARGS="test provider_minimax_integration"

# Run all MiniMax tests
make test-run ARGS="test minimax"
```

---

## Implementation Order

1. **Create `src/provider_minimax.ml`** - Core implementation
   - Message conversion
   - Response parsing
   - Non-streaming completion
   - Streaming completion

2. **Update `src/provider.ml`** - Add MiniMax to type and detection

3. **Update `src/provider_init.ml`** - Register handlers

4. **Update `src/models_catalog.ml`** - Add model entries

5. **Update `src/cost_tracker.ml`** - Add pricing

6. **Update `docs/models.csv`** - Add model documentation

7. **Create `test/test_provider_minimax.ml`** - Unit tests

8. **Create integration tests** - Live API tests

9. **Verify**: `make test` and manual testing

---

## Verification Commands

```bash
# Build
make build

# Run unit tests
make test

# Run MiniMax-specific tests
make test-run ARGS="test provider_minimax"

# Run with MiniMax API key for integration tests
MINIMAX_API_KEY=your_key make test-run ARGS="test minimax_integration"

# Format check
make fmt-check
```

---

## Notes

- MiniMax's Anthropic-compatible endpoint is recommended over OpenAI-compatible due to thinking support
- The `thinking_budget_tokens` provider config option can control thinking budget (passed as `thinking.budget_tokens`)
- Known issue: Simple function calling may benefit from disabling thinking (see GitHub issue #77)
- MiniMax-M2.7-highspeed is 2x faster but 2x the cost per token
