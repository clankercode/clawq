# Z.AI Web Reader Direct HTTP API

Source: `https://docs.z.ai/api-reference/tools/web-reader`
Fetched: 2026-03-10

Direct REST API details from the official page:

- Method/path: `POST /paas/v4/reader`
- Base URL: `https://api.z.ai/api`
- Full URL: `https://api.z.ai/api/paas/v4/reader`
- Auth: `Authorization: Bearer <api_key>`

Request fields called out in docs:

- `url` - required
- `timeout` - optional, default `20`
- `no_cache` - optional, default `false`
- `return_format` - optional, default `markdown`
- `retain_images` - optional, default `true`
- `no_gfm` - optional, default `false`
- `keep_img_data_url` - optional, default `false`
- `with_images_summary` - optional, default `false`
- `with_links_summary` - optional, default `false`

Response fields called out in docs:

- top-level: `id`, `created`, `request_id`, `model`, `reader_result`
- `reader_result`: `content`, `description`, `title`, `url`, `external`, `metadata`

Important difference from MCP:

- This is a plain REST tool API, not MCP transport
- Useful if you want richer reader options than the minimal MCP wrapper arguments
