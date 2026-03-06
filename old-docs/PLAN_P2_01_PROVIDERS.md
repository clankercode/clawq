# P2-01: Native LLM Provider Integrations

## Context

clawq currently uses a single OpenAI-compatible abstraction (`provider.ml`) with hard-coded base URLs for known providers (Z.ai, OpenRouter, Groq). Any provider not exposed as OpenAI-compatible (Anthropic, Gemini, Vertex AI, etc.) requires routing through OpenRouter, adding latency, cost, and an unnecessary intermediary.

nullclaw has 23+ native provider integrations with provider-specific handling (Anthropic Messages API, Gemini GenerateContent, Vertex AI service-account auth, Ollama native API, etc.). This plan closes that gap by adding native provider modules to clawq.

## Scope

Providers to add natively (in priority order):
1. **Anthropic** — Messages API (`/v1/messages`), `x-api-key` + API-version headers, native tool-calling format
2. **Gemini** — GenerateContent API, `AIzaSy*` key prefix, streaming, function declarations
3. **Vertex AI** — Service-account JWT auth, OAuth token refresh, project/location endpoint resolution
4. **Ollama** — `http://localhost:11434` default, `/api/chat` endpoint, native streaming format
5. **Mistral** — OpenAI-compatible, but needs explicit base URL + key validation
6. **xAI** (Grok) — OpenAI-compatible via `api.x.ai`
7. **DeepSeek** — OpenAI-compatible via `api.deepseek.com`
8. **Cohere** — Command R+ native (`/v2/chat`)
9. **AWS Bedrock** — Not feasible without AWS SDK; stub with clear error

## Approach

### 1. Provider interface abstraction

Introduce a `Provider_backend.ml` with a polymorphic record type (similar to Channel.S):

```ocaml
type backend = {
  name : string;
  complete :
    config:Runtime_config.provider_config ->
    request:request ->
    completion Lwt.t;
  stream :
    config:Runtime_config.provider_config ->
    request:request ->
    on_chunk:(string -> unit) ->
    completion Lwt.t;
}
```

Keep the existing `provider.ml` as the public API; it dispatches to the right backend based on `provider_kind`.

### 2. Provider detection

Add `provider_kind_of_config` in `provider.ml`:
- Key prefix `sk-ant-*` → Anthropic
- Key prefix `AIzaSy*` → Gemini
- Key prefix `google-cloud-*` or `service_account_json` field → Vertex
- `base_url` contains `localhost:11434` or `ollama` → Ollama
- Explicit `kind` field in config → use that
- Fallback → OpenAI-compatible (existing behavior)

### 3. Per-provider modules (new files)

| File | Provider | Protocol |
|------|----------|----------|
| `src/provider_anthropic.ml` | Anthropic | POST `/v1/messages`, streaming SSE, tools as `tool_choice` |
| `src/provider_gemini.ml` | Gemini | POST `/v1beta/models/{model}:generateContent`, function declarations |
| `src/provider_vertex.ml` | Vertex AI | Service-account JWT (HS256), OAuth token refresh, endpoint resolution |
| `src/provider_ollama.ml` | Ollama | POST `/api/chat`, streaming NDJSON (not SSE) |
| `src/provider_cohere.ml` | Cohere | POST `/v2/chat`, tool-use format |

Mistral, xAI, DeepSeek: handled via existing OpenAI-compatible path with explicit `base_url` defaults — add to `Runtime_config` provider URL map.

### 4. Anthropic native (highest priority)

Request format:
```json
POST /v1/messages
x-api-key: {api_key}
anthropic-version: 2023-06-01
Content-Type: application/json

{
  "model": "claude-3-5-sonnet-20241022",
  "max_tokens": 8192,
  "system": "...",
  "messages": [...],
  "tools": [{"name": ..., "description": ..., "input_schema": ...}]
}
```

Response: `content[].type` = `"text"` or `"tool_use"` (not `tool_calls`).
Streaming: SSE events `content_block_delta` with `delta.type = "text_delta"` or `"input_json_delta"`.

Translation layer: `Provider.message` ↔ Anthropic messages format (system prompt extracted from message list).

### 5. Gemini native

Request: `POST /v1beta/models/{model}:generateContent` with `contents[]`, `systemInstruction`, `tools[].functionDeclarations`.
Streaming: `streamGenerateContent` endpoint, returns `candidates[0].content` deltas.
Key detection: `AIzaSy` prefix.

### 6. Vertex AI

Auth: Read service-account JSON (path or inline in config), sign JWT, exchange for OAuth access token, cache with 120s safety margin before expiry.
Endpoint: `https://{location}-aiplatform.googleapis.com/v1/projects/{project}/locations/{location}/publishers/google/models/{model}:streamGenerateContent`
Fallback: `VERTEX_API_KEY`, `VERTEX_OAUTH_TOKEN`, `GOOGLE_OAUTH_ACCESS_TOKEN` env vars.

### 7. Runtime config changes

Add to `Runtime_config.provider_config`:
```ocaml
kind : string option;          (* "anthropic" | "gemini" | "vertex" | "ollama" | ... *)
service_account_json : string option;  (* for Vertex *)
project_id : string option;    (* for Vertex *)
location : string option;      (* for Vertex, default "us-central1" *)
```

### 8. Config loader changes

`config_loader.ml`: parse new optional fields from provider JSON object; backward-compatible.

### 9. dune changes

Add new files to `clawq_runtime_core` modules list in `src/dune`. No new dependencies needed (all HTTP via existing `cohttp-lwt-unix`). JWT signing: use existing `mirage-crypto` (HMAC-SHA256 for service-account JWTs).

## Files to Create/Modify

- **Create**: `src/provider_anthropic.ml`
- **Create**: `src/provider_gemini.ml`
- **Create**: `src/provider_vertex.ml`
- **Create**: `src/provider_ollama.ml`
- **Create**: `src/provider_cohere.ml`
- **Modify**: `src/provider.ml` — add dispatch by kind, keep existing OpenAI path as default
- **Modify**: `src/runtime_config.ml` — new provider config fields
- **Modify**: `src/config_loader.ml` — parse new fields
- **Modify**: `src/dune` — add new modules to `clawq_runtime_core`
- **Modify**: `test/test_main.ml` — add provider dispatch tests, Anthropic message format tests

## Test Strategy

1. **Unit tests** (no network):
   - Provider kind detection from API key prefix and config fields
   - Anthropic message format conversion (system extraction, tool-use format)
   - Gemini request/response format translation
   - Vertex JWT construction (sign with test key, verify fields)
   - Ollama NDJSON streaming parser

2. **Integration smoke tests** (skipped if no key in env):
   - `ANTHROPIC_API_KEY` set → `cmd_doctor` reports Anthropic native
   - `GEMINI_API_KEY` set → provider resolves to Gemini backend

3. Run: `make test` after each new provider module

## Dependencies

- None new; all HTTP via `cohttp-lwt-unix`, crypto via `mirage-crypto`
- Vertex JWT: HMAC-SHA256 via `digestif` (already linked)

## Order of Implementation

1. Add `provider_kind` detection + dispatch skeleton to `provider.ml`
2. Implement `provider_anthropic.ml` (most commonly used)
3. Implement `provider_ollama.ml` (local inference, no auth complexity)
4. Implement `provider_gemini.ml`
5. Implement `provider_vertex.ml` (most complex auth)
6. Implement `provider_cohere.ml`
7. Add explicit URL defaults for Mistral/xAI/DeepSeek in runtime_config
