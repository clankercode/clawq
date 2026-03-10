# Z.AI Web Search MCP Server

Source: `https://docs.z.ai/devpack/mcp/search-mcp-server`
Fetched: 2026-03-10

Key points captured from the official page:

- Remote MCP server for GLM Coding Plan users
- Tool provided: `webSearchPrime`
- Remote endpoint: `https://api.z.ai/api/mcp/web_search_prime/mcp`
- Auth header: `Authorization: Bearer <api_key>`
- OpenCode example:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "web-search-prime": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
      "headers": {
        "Authorization": "Bearer your_api_key"
      }
    }
  }
}
```

- Generic MCP client example uses streamable HTTP:

```json
{
  "mcpServers": {
    "web-search-prime": {
      "type": "streamable-http",
      "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
      "headers": {
        "Authorization": "Bearer your_api_key"
      }
    }
  }
}
```

- Older-client SSE fallback:

```json
{
  "mcpServers": {
    "web-search-prime": {
      "type": "sse",
      "url": "https://api.z.ai/api/mcp/web_search_prime/sse?Authorization=your_api_key"
    }
  }
}
```

- Troubleshooting highlights: invalid API key, timeout, empty search results
- Quota note from page: search/reader calls share GLM Coding Plan quota

## Root Cause: HTTP 400 on Connect (B327)

clawq's MCP HTTP client was getting HTTP 400 from these endpoints because it did
not send the `Accept: application/json, text/event-stream` header required by the
MCP Streamable HTTP specification (2025-03-26).

**Fix (landed in B327):** `default_http_post` now always includes this header.
The client also handles `text/event-stream` response bodies by extracting JSON
from SSE `data:` lines, since the server may reply with either content-type.

Config for clawq (`~/.clawq/mcp_servers.json`):

```json
[
  {
    "name": "zai-web-search",
    "url": "https://api.z.ai/api/mcp/web_search_prime/mcp",
    "headers": { "Authorization": "Bearer YOUR_API_KEY" }
  }
]
```
