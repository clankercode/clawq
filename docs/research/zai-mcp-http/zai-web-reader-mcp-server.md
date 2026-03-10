# Z.AI Web Reader MCP Server

Source: `https://docs.z.ai/devpack/mcp/reader-mcp-server`
Fetched: 2026-03-10

Key points captured from the official page:

- Remote MCP server for GLM Coding Plan users
- Tool provided: `webReader`
- Remote endpoint: `https://api.z.ai/api/mcp/web_reader/mcp`
- Auth header: `Authorization: Bearer <api_key>`
- OpenCode example:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "web-reader": {
      "type": "remote",
      "url": "https://api.z.ai/api/mcp/web_reader/mcp",
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
    "web-reader": {
      "type": "streamable-http",
      "url": "https://api.z.ai/api/mcp/web_reader/mcp",
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
    "web-reader": {
      "type": "sse",
      "url": "https://api.z.ai/api/mcp/web_reader/sse?Authorization=your_api_key"
    }
  }
}
```

- Tool returns page title, main content, metadata, links, and related structured data
- Troubleshooting highlights: invalid token, timeout, webpage fetch failure
- Quota note from page: search/reader calls share GLM Coding Plan quota

## Root Cause: HTTP 400 on Connect (B327)

Same root cause as zai-web-search: missing `Accept: application/json,
text/event-stream` header required by MCP Streamable HTTP (2025-03-26 spec).

**Fix (landed in B327):** `default_http_post` now always sends this header and
the client parses SSE `data:` lines when the server returns `text/event-stream`.

Config for clawq (`~/.clawq/mcp_servers.json`):

```json
[
  {
    "name": "zai-web-reader",
    "url": "https://api.z.ai/api/mcp/web_reader/mcp",
    "headers": { "Authorization": "Bearer YOUR_API_KEY" }
  }
]
```
