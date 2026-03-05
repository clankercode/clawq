# GitHub REST API Rate Limits

## Primary Rate Limits

Primary limits apply to the total number of API requests per hour.

| Authentication Type | Requests / Hour |
|--------------------|----------------|
| Unauthenticated | 60 |
| Authenticated (PAT or OAuth) | 5,000 |
| GitHub App installation (non-Enterprise) | up to 12,500 (scales with repo/user count) |
| GitHub App owned by Enterprise Cloud org | 15,000 |
| OAuth App owned/approved by Enterprise Cloud org | 15,000 |
| GitHub Actions `GITHUB_TOKEN` | 1,000 per repo per hour |

The primary rate limit window resets each hour. The reset time is available in the `x-ratelimit-reset` response header.

---

## Secondary Rate Limits

Secondary limits are separate from the primary hourly quota and exist to prevent abuse. They are not counted in `x-ratelimit-remaining`.

| Constraint | Limit |
|------------|-------|
| Concurrent requests | 100 |
| Content-generating requests | 80 per minute, 500 per hour |
| CPU time (server-side) | 90 seconds per 60 real-time seconds |

**Content-generating requests** include:
- `POST` requests that create resources (issues, comments, PRs, reviews)
- `PATCH` or `PUT` requests that update content
- Any mutation that causes GitHub to generate a notification or webhook event

Posting comments on issues and PRs is a content-generating operation. At scale, stay well under 80 comments/minute.

### Secondary Rate Limit Errors

When a secondary limit is hit, the API returns either:
- `403 Forbidden` with a JSON body describing the violation
- `429 Too Many Requests`

The `retry-after` header (in seconds) is present in the response when a secondary limit applies. Always respect it.

---

## Rate Limit Response Headers

Every GitHub REST API response includes rate limit headers:

| Header | Type | Description |
|--------|------|-------------|
| `x-ratelimit-limit` | integer | Maximum requests allowed in the current window |
| `x-ratelimit-remaining` | integer | Requests left in the current window |
| `x-ratelimit-reset` | integer | Unix epoch timestamp (UTC) when the window resets |
| `x-ratelimit-used` | integer | Requests consumed so far in the current window |
| `x-ratelimit-resource` | string | Which rate limit bucket this request counted against |
| `retry-after` | integer | Seconds to wait before retrying (secondary limits only; not always present) |

### x-ratelimit-resource Values

| Value | Meaning |
|-------|---------|
| `core` | Standard REST API requests |
| `search` | Search API (`/search/*`) |
| `graphql` | GraphQL API |
| `integration_manifest` | GitHub App manifest flow |
| `code_scanning_upload` | Code scanning results upload |

---

## Checking Rate Limit Status

You can proactively check your current rate limit status without consuming a request:

```
GET https://api.github.com/rate_limit
```

Response:
```json
{
  "resources": {
    "core": {
      "limit": 5000,
      "remaining": 4998,
      "reset": 1372700873,
      "used": 2
    },
    "search": {
      "limit": 30,
      "remaining": 18,
      "reset": 1372697452,
      "used": 12
    },
    "graphql": {
      "limit": 5000,
      "remaining": 4993,
      "reset": 1372700389,
      "used": 7
    }
  },
  "rate": {
    "limit": 5000,
    "remaining": 4998,
    "reset": 1372700873,
    "used": 2
  }
}
```

---

## Handling Rate Limits in Client Code

### Primary Limit Strategy

1. After each response, read `x-ratelimit-remaining`.
2. If `remaining` is 0, compute wait time: `reset_timestamp - now_unix_seconds + 1`.
3. Sleep until the window resets, then retry.

### Secondary Limit Strategy

1. Check for `retry-after` header in any `403` or `429` response.
2. If present, sleep for `retry-after` seconds then retry once.
3. If absent on a `403`, inspect the body for a rate limit message; if detected, wait 60 seconds before retrying.
4. Implement exponential backoff for repeated failures.

### Recommended Backoff Pattern

```
base_delay = 1.0  (* seconds *)
max_delay  = 64.0 (* seconds *)
attempt    = 0

on_rate_limit_error:
  delay = min(base_delay * 2^attempt, max_delay)
  sleep(delay + jitter)
  attempt++
```

### Warning

Continuing to send requests while rate-limited can result in the permanent banning of your API token or IP address from the GitHub API.

---

## Relevance for Comment-Posting Workloads

For a bot that posts comments on issues and PRs:
- The 80 content-generating requests/minute secondary limit is the binding constraint for burst behavior.
- At steady state with 5,000 requests/hour primary, you have ~83 requests/minute — approximately matching the secondary limit.
- Practical safe throughput for comment-posting: **1 comment per second** (60/min), leaving headroom for other requests.
- Always check `x-ratelimit-remaining` and back off when below a threshold (e.g., < 100).
