# Z.AI Web Search Direct HTTP API

Source: `https://docs.z.ai/api-reference/tools/web-search`
Fetched: 2026-03-10

Direct REST API details from the official page:

- Method/path: `POST /paas/v4/web_search`
- Base URL: `https://api.z.ai/api`
- Full URL: `https://api.z.ai/api/paas/v4/web_search`
- Auth: `Authorization: Bearer <api_key>`
- Optional header: `Accept-Language: en-US,en`

Request fields called out in docs:

- `search_engine` - required, documented value `search-prime`
- `search_query` - required
- `count` - optional, `1-50`, default `10`
- `search_domain_filter` - optional domain whitelist
- `search_recency_filter` - optional: `oneDay`, `oneWeek`, `oneMonth`, `oneYear`, `noLimit`
- `request_id` - optional
- `user_id` - optional end-user identifier

Response fields called out in docs:

- top-level: `id`, `created`, `search_result`
- each result: `title`, `content`, `link`, `media`, `icon`, `refer`, `publish_date`

Important difference from MCP:

- This is a plain REST tool API, not MCP transport
- Useful if you want search without MCP server discovery/tool wiring
