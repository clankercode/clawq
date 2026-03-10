# Z.AI MCP HTTP / Remote Usage Notes

Fetched on 2026-03-10.

This directory captures the Z.AI docs relevant to replacing a local `npx`/stdio MCP setup with Z.AI-hosted remote MCP over HTTP for web search and web page fetch/reader access.

## What was already in this repo

The repo did not already contain the official Z.AI MCP HTTP docs.

Existing repo references were limited to:

- `src/tools_builtin.ml` hardcoded remote MCP endpoints and JSON-RPC tool calls
- `docs/public/llms-full.txt` config/tool notes for `zai_mcp`
- `src/ZAI_API_REQUIREMENTS.md` for the OpenAI-compatible chat API, not MCP transport docs

## Official source pages saved here

- `zai-web-search-mcp-server.md` - official remote MCP setup for `webSearchPrime`
- `zai-web-reader-mcp-server.md` - official remote MCP setup for `webReader`
- `zai-web-search-api.md` - direct HTTP API reference for `/paas/v4/web_search`
- `zai-web-reader-api.md` - direct HTTP API reference for `/paas/v4/reader`
- `opencode-mcp-servers.md` - OpenCode local vs remote MCP config docs

## Key transport details

- Web search remote MCP endpoint: `https://api.z.ai/api/mcp/web_search_prime/mcp`
- Web reader remote MCP endpoint: `https://api.z.ai/api/mcp/web_reader/mcp`
- Auth: `Authorization: Bearer <api_key>`
- Remote transport names in docs: `http`, `streamable-http`, `streamableHttp`, and for OpenCode `type: "remote"`
- SSE fallback documented for older clients:
  - `https://api.z.ai/api/mcp/web_search_prime/sse?Authorization=<api_key>`
  - `https://api.z.ai/api/mcp/web_reader/sse?Authorization=<api_key>`
- Availability: documented as a GLM Coding Plan feature with shared quota for search/reader usage

## OpenCode config shape

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "web-search-prime": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
      "headers": {
        "Authorization": "Bearer {env:ZAI_API_KEY}"
      }
    },
    "web-reader": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_reader/mcp",
      "headers": {
        "Authorization": "Bearer {env:ZAI_API_KEY}"
      }
    }
  }
}
```

## Implementation-critical notes

- If replacing local `npx` MCP, the config change is from `type: "local"` plus `command: [...]` to `type: "remote"` plus `url` and `headers`.
- No local server process is started for remote MCP.
- The official docs describe remote MCP discovery/config, but not raw `tools/call` HTTP bodies. The current clawq implementation already uses the standard JSON-RPC pattern:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "webSearchPrime",
    "arguments": { "query": "..." }
  }
}
```

- Current clawq code posts that JSON-RPC body directly to the remote MCP endpoint, which matches the intended remote MCP model.
- Tool names differ from direct REST APIs:
  - MCP search tool: `webSearchPrime`
  - MCP reader tool: `webReader`
  - Direct REST endpoints: `/paas/v4/web_search` and `/paas/v4/reader`
- If using OpenCode remote MCP, tool naming will be prefixed by the MCP server name in OpenCode's tool registry.
- If using direct REST instead of MCP, request/response schemas differ and you lose MCP tool discovery semantics.
