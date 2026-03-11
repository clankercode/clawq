# GitHub Webhook + Cloudflare Tunnel Setup

This guide walks through setting up clawq to receive GitHub webhooks via a Cloudflare named tunnel, so the bot can respond to `/clawq` commands and run GitHub event hooks.

## Prerequisites

- **cloudflared** installed ([download](https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/))
- **GitHub PAT** with `repo` scope (Settings > Developer settings > Personal access tokens)
- clawq built and working (`make build`)

## 1. Cloudflare Named Tunnel Setup

### Authenticate cloudflared

```bash
cloudflared login
```

This opens a browser to authorize cloudflared with your Cloudflare account.

### Create a tunnel

```bash
cloudflared tunnel create clawq-webhook
```

Note the tunnel UUID printed in the output.

### Route DNS to the tunnel

```bash
cloudflared tunnel route dns clawq-webhook clawq-webhook.yourdomain.com
```

This creates a CNAME record pointing `clawq-webhook.yourdomain.com` to the tunnel.

### Create cloudflared config

Create `~/.cloudflared/config.yml` (or a custom path):

```yaml
tunnel: <TUNNEL_UUID>
credentials-file: /home/you/.cloudflared/<TUNNEL_UUID>.json

ingress:
  - hostname: clawq-webhook.yourdomain.com
    service: http://localhost:8080
  - service: http_status:404
```

The `service` port must match the clawq gateway port.

## 2. clawq Configuration

Add tunnel and GitHub sections to `~/.clawq/config.json`:

```json
{
  "gateway": {
    "port": 8080,
    "host": "127.0.0.1"
  },
  "tunnel": {
    "enabled": true,
    "managed": true,
    "tunnel_name": "clawq-webhook",
    "config_dir": "/home/you/.cloudflared"
  },
  "channels": {
      "github": {
        "auth": {
          "type": "pat",
          "token": "ghp_xxxxxxxxxxxxxxxxxxxx"
        },
        "repos": [
          {
            "name": "myorg/myrepo",
          "webhook_secret": "a-strong-random-secret",
          "webhook_path": "/webhooks/github/myrepo",
          "allow_users": ["*"],
          "react_to": ["issue_comment", "pull_request_review_comment"],
          "include_pr_files": true
        }
      ]
    }
  }
}
```

### Configuration reference

| Field | Description |
|-------|-------------|
| `tunnel.managed` | `true` to have clawq spawn and manage cloudflared |
| `tunnel.tunnel_name` | Name passed to `cloudflared tunnel run` |
| `tunnel.config_dir` | Directory containing cloudflared config and credentials |
| `tunnel.url` | Static tunnel URL (alternative to managed mode) |
| `channels.github.auth.type` | Authentication type, currently only `"pat"` |
| `channels.github.auth.token` | GitHub Personal Access Token |
| `channels.github.repos[].name` | Required `owner/repo` identity check for the webhook path; payload repo must match |
| `channels.github.repos[].webhook_secret` | Shared secret for HMAC verification |
| `channels.github.repos[].webhook_path` | URL path the webhook POSTs to; must be unique across configured repos |
| `channels.github.repos[].allow_users` | List of GitHub usernames, or `["*"]` for all |
| `channels.github.repos[].react_to` | Event types to handle. Empty list means all webhook events configured for the repo path. |
| `channels.github.repos[].include_pr_files` | Fetch changed file list for context |

## 2.1 Optional GitHub hooks

clawq can also run automation hooks from `~/.clawq/workspace/gh-hooks/`.

Each hook is a markdown file with YAML-style frontmatter. The current implementation requires:

- `repo: owner/repo`
- `event: <github webhook event>`
- optional `match:` fields for exact matching
- markdown body used as the initial agent prompt

Example `~/.clawq/workspace/gh-hooks/workflow_job.md`:

```md
---
name: investigate-failed-job
repo: myorg/myrepo
event: workflow_job
match:
  status: completed
  conclusion: failure
---
Investigate this failed CI job for {{repo}} on {{branch}}.

Saved payload: {{payload_path}}

Job details:
{{json raw.workflow_job}}
```

Notes:

- Hooks are matched in filename order.
- Hook payload snapshots are stored under `~/.clawq/workspace/tmp/github-deliveries/`.
- Old payload snapshots are cleaned up opportunistically when new GitHub webhooks arrive.
- For user-generated events such as comments, reviews, issues, and PR bodies, `allow_users` still applies before hooks are allowed to run.

## 3. GitHub Webhook Registration

1. Go to your repo's **Settings > Webhooks > Add webhook**
2. **Payload URL**: `https://clawq-webhook.yourdomain.com/webhooks/github/myrepo`
   - Must match `webhook_path` in your config
3. **Content type**: `application/json`
4. **Secret**: Same value as `webhook_secret` in config
5. **Events**: Select individual events that match the commands/hooks you want.
   Common choices:
   - Pull requests
   - Issue comments
   - Pull request review comments
   - Pull request reviews
   - Issues
   - Workflow jobs
   - Workflow runs
   - Check runs
   - Check suites
   - Pushes
6. **Active**: checked

## 4. Testing

1. Start clawq in daemon mode (it will spawn cloudflared automatically if `managed: true`)
2. Open or comment on a PR in the configured repo
3. Post a comment containing `/clawq hello` at the start of a line
4. The bot should reply with a quoted command and response
5. If you configured a hook, trigger the matching webhook event and inspect clawq logs for `GitHub hooks:` messages

Example comment:
```
Looks good overall.

/clawq summarize the changes in this PR
```

## 5. Troubleshooting

### Signature mismatch (403 Forbidden)

- Verify the `webhook_secret` in clawq config matches the secret in GitHub webhook settings exactly
- Check there are no trailing spaces or newlines in the secret

### Tunnel not running / webhook not received

- Check cloudflared logs: `journalctl -u cloudflared` or daemon stderr
- Verify DNS resolves: `dig clawq-webhook.yourdomain.com`
- Test the health endpoint: `curl https://clawq-webhook.yourdomain.com/health`
- If using managed mode, check clawq logs for "Connection registered" messages

### Wrong event type / no response

- Ensure the GitHub webhook is configured to send the correct event types
- Check `react_to` in config; an empty list accepts all event types delivered to that repo path
- `/clawq` replies still require a supported comment/PR event containing `/clawq` at the start of a line
- Hook-driven automation additionally requires a matching file in `~/.clawq/workspace/gh-hooks/`
- Check clawq logs for `GitHub hooks:` match, render, and snapshot messages

### User not allowed

- Check `allow_users` in config; set to `["*"]` to allow all users
- If restricted, the commenting user's GitHub username must be in the list
- Only a small system-event subset such as CI/check hooks bypasses `allow_users`
- Events with arbitrary user-controlled text, including `push`, should be treated as gated by `allow_users`
- Check clawq logs for "ignoring event ... from unauthorized user" messages

### Bot replying to itself

- Add the bot's GitHub username to an exclusion list, or avoid using `["*"]` for `allow_users` and instead list specific human users
