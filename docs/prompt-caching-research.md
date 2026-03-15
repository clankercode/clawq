# OpenAI / Codex Prompt Caching: Research & clawq Analysis

## 1. How OpenAI Prompt Caching Works

OpenAI prompt caching is **automatic** — no opt-in required. When a request contains a prefix the system has recently processed, OpenAI routes it to a server that already has that prefix's KV tensors in memory, avoiding recomputation.

### Core Mechanism: Exact Prefix Matching

- Cache hits require an **exact byte-for-byte prefix match** from the start of the serialized prompt.
- Minimum prompt size: **1024 tokens**. Cache hits occur in **128-token increments** — only complete 128-token blocks are reused.
- The system hashes the initial ~256 tokens to route requests to the same inference server.
- Any change to any token before the divergence point invalidates the entire cached prefix from that point onward.

### What Is Cacheable

The entire serialized request prefix is cacheable, including:
- System / developer instructions
- Tool definitions (names, descriptions, parameter schemas)
- Structured output schemas
- Messages array (all roles)
- Images (base64 or URL — must use identical `detail` settings)

Tool definitions and the messages array are serialized together into the prompt prefix. **Their order matters** — reordering tools or messages breaks the prefix match.

### Cache Retention

| Policy | Duration | Availability |
|--------|----------|-------------|
| In-memory (default) | 5–10 min of inactivity, max 1 hour | All models |
| Extended (`"24h"`) | Up to 24 hours (KV tensors offloaded to GPU-local storage) | GPT-5.4, GPT-5.2, GPT-5.1 variants, GPT-4.1 |

Configure via `prompt_cache_retention` parameter on the request.

### Responses API vs Chat Completions

- The Responses API shows **40–80% better cache utilization** than Chat Completions for reasoning models, because `previous_response_id` preserves chain-of-thought tokens between turns. Chat Completions drops these.
- For non-reasoning models, caching behavior is equivalent.

### Cost Discounts on Cached Tokens

| Model family | Discount |
|-------------|----------|
| gpt-4o | 50% |
| gpt-4.1 | 75% |
| gpt-5-nano | 90% |

### Routing Parameters

- **`prompt_cache_key`**: A routing hint combined with the prefix hash. Steers requests with similar prefixes to the same server. One customer improved hit rate from 60% → 87%. Optimal at ≤15 RPM per unique key.
- **`prompt_cache_retention`**: `"in_memory"` (default) or `"24h"`.

## 2. What Must Remain Stable for Cache Hits

Cache hits depend on an **identical prefix** from the start of the serialized request. The following must not change between requests within a session:

| Component | Must be stable? | Notes |
|-----------|----------------|-------|
| Instructions / system prompt | **YES** — first in prefix | Any change (even whitespace) breaks all cache |
| Tool definitions | **YES** — order and content | Reordering or schema changes invalidate |
| Tool ordering | **YES** | Same tools in same order |
| Earlier messages (history) | **YES** — must be append-only | Modifying/removing earlier messages breaks prefix |
| Model name | **YES** | Different model = different routing |
| Temperature / params | Depends on serialization position | If before messages in body, changes break prefix |
| Structured output schema | **YES** — if present | Prefixed to system message |

### What Causes Cache Misses

1. **Dynamic content in system prompt**: Timestamps, current dates, directory listings, session IDs
2. **Tool definition changes**: Adding/removing/reordering tools, schema changes
3. **History mutation**: Editing or removing earlier messages instead of appending
4. **Parameter changes**: `reasoning_effort`, temperature changes between turns
5. **Context compaction**: Replacing early history with summaries shifts the entire prefix
6. **Idle gaps**: >5–10 min between requests causes in-memory eviction

## 3. Analysis of clawq's Request Assembly Path

### 3a. OpenAI-Compatible Path (Chat Completions — `provider.ml`)

Request body assembly (`provider.ml:714-732` for non-streaming, `:1089-1109` for streaming):

```
{
  "model": <model>,
  "messages": [system_msg, ...history_msgs],
  "temperature": <float>,
  "stream": true/false,
  [optional] "tools": [...],
  [optional] "reasoning_effort": "...",
  [optional] provider_extra_body_fields
}
```

**System prompt** is rebuilt every turn via `Prompt_builder.build` (`agent.ml:116-118`). This is called inside `build_messages` on every request.

**Runtime context** is injected into the **last user message** via `inject_runtime_context` (`agent.ml:105-113`). This prepends the runtime context block to the last user message's content.

### 3b. Codex / Responses API Path (`provider_openai_codex.ml`)

Request body assembly (`provider_openai_codex.ml:378-422`):

```
{
  "model": <model>,
  "input": [flat item array],
  "instructions": <concatenated system messages>,
  "stream": true,
  "store": false,
  "parallel_tool_calls": true,
  [optional] "tools": [...]
}
```

Key observations:
- System messages are extracted into `instructions` field (`extract_instructions`, line 271-283)
- History is converted to flat Responses API items via `messages_to_input`
- `store` is hardcoded to `false` — no `previous_response_id` chaining
- No `prompt_cache_retention` parameter is sent
- No `prompt_cache_key` parameter is sent

## 4. Identified Sources of Cache Misses in clawq

### CRITICAL: System prompt rebuilt every turn (Both paths)

`build_messages` (`agent.ml:116-118`) calls `Prompt_builder.build` on **every turn**, which regenerates the entire system prompt. The prompt builder (`prompt_builder.ml:395-533`) includes:

- Workspace doc blocks (`EGO.md`, `AGENTS.md`, `MEMORY.md`, etc.) — **read fresh from disk each turn**. If any workspace file changes (even a single byte), the system prompt changes.
- Tool descriptions block — regenerated from registry each turn. Tool ordering depends on `List.sort` by name, which is at least deterministic.
- Build info (version, git hash, build date) — stable within a binary.
- Various config-driven sections (autonomy, safety, workspace, operating stance).

**Impact**: If workspace files are stable and config doesn't change, the system prompt should be identical between turns. **This is cache-friendly by default** when workspace files are stable, but fragile — any workspace file edit mid-session breaks the entire prefix.

### CRITICAL: Runtime context injected into last user message (Both paths)

`inject_runtime_context` (`agent.ml:105-113`) prepends volatile runtime context into the **last user message**. This runtime context (`prompt_builder.ml:217-260`) includes:

- **Current UTC time** (`now_utc_iso8601()`) — changes every second
- **Local time** — changes every second
- **Directory contents** (`list_cwd_entries()`) — changes if files are created/deleted
- **Git branch** — can change
- Session details (id, workspace, sandbox info, etc.)
- Background task summaries
- Context usage stats
- Skill listings

**Impact on caching**: Since this is injected into the **last** user message, it does NOT break the prefix cache for earlier messages. The cache can still match the prefix up to (but not including) the last user message. **This is actually well-designed for caching** — volatile data is pushed to the end.

### MODERATE: No `previous_response_id` (Codex path)

The Codex path uses `store: false` and doesn't chain responses via `previous_response_id`. This means:

- Chain-of-thought / reasoning tokens from previous turns are **not preserved**
- The 40–80% cache improvement for reasoning models is **not realized**
- Each request must re-serialize the full conversation history

**Impact**: For reasoning models (o3, o4-mini, GPT-5 with reasoning), this is a significant missed optimization. For non-reasoning models (gpt-5.x-codex), the impact is minimal.

### MODERATE: No `prompt_cache_key` (Both paths)

Neither path sends `prompt_cache_key`. For sessions with steady traffic patterns, this means the routing hash is based solely on the prefix ~256 tokens. This is likely fine for single-session use but suboptimal for:

- Multiple concurrent sessions sharing the same system prompt
- High-throughput daemon scenarios

### MODERATE: No `prompt_cache_retention` (Both paths)

Neither path sends `prompt_cache_retention: "24h"`. The default in-memory policy evicts after 5–10 min of inactivity. For agent sessions with tool execution pauses >5 min (e.g., long shell commands, waiting for user input), the cache will be evicted.

### LOW: Tool definitions ordering

Tool definitions in `prompt_builder.ml:317-328` are sorted by `String.compare` on tool name. This is deterministic and stable. Tools are also converted to Responses API format deterministically in `provider_openai_codex.ml:164-191`. **This is cache-friendly.**

### LOW: History is append-only

Agent history is maintained as a prepend-to-head list (`agent.ml:1973-1976`), reversed when building messages (`agent.ml:121`). Messages are only **appended** (never modified or reordered). **This is cache-friendly.**

### N/A: Background / delegated task flows

Background tasks (`src/background_task.ml`) spawn **external CLI processes** — `codex`, `claude`, `kimi`, `gemini`, `opencode`, `cursor` — via `Process_group.start` with constructed argv arrays (e.g., `codex exec --model ... --dangerously-bypass-approvals-and-sandbox <prompt>`). These processes manage their own request assembly entirely outside clawq's provider pipeline (`provider.ml` / `provider_openai_codex.ml`). clawq has no control over their prompt construction or caching behavior.

**Impact**: Background task caching is governed by the external tool's own implementation (e.g., OpenAI's Codex CLI handles its own prompt assembly and likely benefits from the same automatic OpenAI caching). clawq cannot influence caching for these flows. The recommendations in this document apply only to clawq's own direct LLM request paths (main session turns, daemon-managed sessions).

### LOW: Compaction breaks the prefix

When history is compacted (summarized to save tokens), early messages are replaced with a summary. This shifts the entire prefix after the system prompt. **This is unavoidable** — compaction is necessary to stay within context limits. Cache misses after compaction are expected and acceptable.

### LOW: Temperature in body prefix

Temperature appears as the third field in the Chat Completions body (`provider.ml:717`). If temperature changes between turns (unlikely but possible via config hot-reload), it would break the cache. Temperature is placed **after** messages in the Responses API body, so this is not an issue there (it's not sent at all for Codex).

## 5. Recommendations

### R1: Send `prompt_cache_retention: "24h"` for Codex requests (High impact, low effort)

Add `("prompt_cache_retention", `String "24h")` to the Codex request body in `build_body`. This extends cache lifetime from 5–10 min to 24 hours, which is critical for agent sessions with tool execution pauses.

**Where**: `provider_openai_codex.ml:409-421`

### R2: Send `prompt_cache_key` per session (Medium impact, low effort)

Use the session key (already available in the call chain) as a `prompt_cache_key`. This improves routing stickiness so sequential turns in the same session hit the same cache server.

**Where**: `provider_openai_codex.ml:build_body` — add parameter; thread session_key through from `do_request`.

### R3: Consider `previous_response_id` chaining for reasoning models (High impact, high effort)

For reasoning models, enabling `store: true` and chaining via `previous_response_id` would realize the 40–80% cache improvement. This requires:
- Storing response IDs per session
- Sending only new messages (not full history) when chaining
- Fallback to full-history mode when chain breaks

**This is a larger architectural change** and should be a separate task.

### R4: Avoid rebuilding system prompt when unchanged (Low impact, low effort)

Currently `build_messages` rebuilds the system prompt every turn. Add a digest check: only rebuild if config or workspace files have changed. The infrastructure for this already exists (`observed_active_workspace_files` in the agent state, `capture_active_workspace_file_state_for_config`).

**Note**: The current rebuild is fast (string concatenation), so the performance gain is negligible. The real benefit is ensuring byte-identical system prompts between turns, which is already the case when inputs are stable.

### R5: Log cache hit rate (Low effort, high observability value)

clawq already parses `cached_tokens` from the usage response (`provider.ml:797-802`, `provider_openai_codex.ml:429-442`). Add a log line or metric tracking the cache hit rate (cached_tokens / prompt_tokens) to make caching behavior observable.

**Where**: `agent.ml` cost tracking path, `agent_cost.ml`, or request_stats recording.

## 6. Summary Assessment

| Aspect | Current Status | Cache Impact |
|--------|---------------|-------------|
| System prompt stability | Good — deterministic when workspace files stable | Prefix preserved |
| Runtime context placement | Good — injected into last user message only | Prefix preserved |
| Tool definition ordering | Good — sorted deterministically | Prefix preserved |
| History management | Good — append-only | Prefix preserved |
| `prompt_cache_retention` | Missing — defaults to 5-10 min | Cache evicted during pauses |
| `prompt_cache_key` | Missing — no routing hint | Suboptimal routing |
| `previous_response_id` | Not used (`store: false`) | Reasoning token cache missed |
| Cache hit observability | Parsed but not logged/tracked | No visibility |

**Overall**: clawq's prompt construction is fundamentally cache-friendly. The system prompt is stable, runtime context is correctly placed at the end, tools are ordered deterministically, and history is append-only. The main improvements are operational: enabling extended cache retention, adding routing hints, and improving observability. The `previous_response_id` optimization is the largest potential win but requires significant implementation work.

## Sources

- [OpenAI Prompt Caching Guide](https://developers.openai.com/api/docs/guides/prompt-caching/)
- [Prompt Caching 201 (Cookbook)](https://developers.openai.com/cookbook/examples/prompt_caching_201/)
- [Prompt Caching 101 (Cookbook)](https://developers.openai.com/cookbook/examples/prompt_caching101/)
- [Codex Prompting Guide](https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide/)
- [OpenAI Prompt Caching Announcement](https://openai.com/index/api-prompt-caching/)
- [Community: How does Prompt Caching work?](https://community.openai.com/t/how-does-prompt-caching-work/992307)
- [Community: Responses API caching cost implications](https://community.openai.com/t/respones-api-how-does-prompt-caching-work-and-its-cost-implications/1306660)
