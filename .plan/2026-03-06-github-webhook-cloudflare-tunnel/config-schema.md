# Config Schema

## New Types in `runtime_config.ml`

```ocaml
(* Auth abstraction — PAT now, GitHub App later *)
type github_auth =
  | GithubPat of string            (* "ghp_..." personal access token *)
  (* Future: | GithubApp of github_app_config *)

type github_app_config = {          (* NOT IN V1 — placeholder for future *)
  app_id : int;
  private_key_path : string;
  installation_id : int;
}

type github_repo_config = {
  name : string;              (* "owner/repo" — for logging/display *)
  webhook_secret : string;    (* HMAC-SHA256 secret set in GitHub webhook settings *)
  webhook_path : string;      (* HTTP path, e.g. "/github/webhook/myrepo" — must be unique *)
  agent_name : string option; (* RESERVED: future model routing — parsed but ignored in v1 *)
  allow_users : string list;  (* GitHub usernames, ["*"] = all *)
  react_to : string list;     (* event types: defaults to all supported if empty *)
                              (* valid: "pull_request", "issue_comment", "pull_request_review_comment" *)
  include_pr_files : bool;    (* fetch changed files list — default true *)
}

type github_config = {
  auth : github_auth;               (* credentials for API calls *)
  repos : github_repo_config list;
}

(* Extended tunnel_config *)
type tunnel_config = {
  provider : string;   (* "cloudflare" *)
  enabled : bool;
  url : string;        (* explicit tunnel URL — takes priority over managed; empty = not set *)
  managed : bool;      (* if true, spawn cloudflared subprocess *)
  tunnel_name : string; (* cloudflared named tunnel name — required if managed = true *)
  config_dir : string;  (* cloudflared config dir, default "~/.cloudflared" *)
}

(* channel_config extended *)
type channel_config = {
  cli : bool;
  telegram : telegram_config option;
  discord : discord_config option;
  slack : slack_config option;
  github : github_config option;   (* NEW *)
}
```

## JSON Config Example

```json
{
  "tunnel": {
    "provider": "cloudflare",
    "enabled": true,
    "url": "",
    "managed": true,
    "tunnel_name": "clawq-prod",
    "config_dir": "~/.cloudflared"
  },
  "channels": {
    "github": {
      "auth": {
        "type": "pat",
        "token": "ghp_xxxxxxxxxxxxxxxxxxxx"
      },
      "repos": [
        {
          "name": "acme/backend",
          "webhook_secret": "whsec_xxxxxxxxxxxx",
          "webhook_path": "/github/webhook/acme-backend",
          "agent_name": null,
          "allow_users": ["*"],
          "react_to": ["pull_request", "issue_comment", "pull_request_review_comment"],
          "include_pr_files": true
        },
        {
          "name": "acme/frontend",
          "webhook_secret": "whsec_yyyyyyyyyyyy",
          "webhook_path": "/github/webhook/acme-frontend",
          "agent_name": null,
          "allow_users": ["alice", "bob"],
          "react_to": ["issue_comment"],
          "include_pr_files": true
        }
      ]
    }
  }
}
```

## Static URL Config Example (production, no managed mode)

```json
{
  "tunnel": {
    "provider": "cloudflare",
    "enabled": true,
    "url": "https://clawq.example.com",
    "managed": false,
    "tunnel_name": "",
    "config_dir": ""
  }
}
```

## Auth Abstraction Design

The `github_auth` type is designed so that adding GitHub App support later requires:
1. Add `GithubApp of github_app_config` variant
2. Add JSON parsing for `"type": "github_app"` in config_loader
3. Add `Github_api.get_installation_token ~app_config` that returns a bearer token
4. `Github_api.auth_headers` already accepts `github_auth` and dispatches — just add the new case

No changes needed to `github_webhook.ml`, `github.ml`, or `http_server.ml`.

## `react_to` Filter

Default (empty list or omitted): all supported event types are handled.

Valid values:
- `"pull_request"` — PR opened, edited, reopened (NOT synchronize, closed, merged)
- `"issue_comment"` — comments on issues AND pull requests
- `"pull_request_review_comment"` — in-diff review comments

## Tunnel URL Resolution (implemented in `cf_tunnel.ml`)

```
Priority:
1. config.tunnel.url  (non-empty string)
2. Sys.getenv "CLAWQ_TUNNEL_URL"
3. If config.tunnel.managed = true:
     spawn cloudflared, parse URL from output
     (returns Lwt promise that resolves when URL is known)
4. None
```

## Defaults

```ocaml
let default_tunnel = {
  provider = "cloudflare";
  enabled = false;
  url = "";
  managed = false;
  tunnel_name = "";
  config_dir = "";
}

let default_github_repo = {
  name = "";
  webhook_secret = "";
  webhook_path = "";
  agent_name = None;
  allow_users = [ "*" ];
  react_to = [];           (* empty = all supported *)
  include_pr_files = true;
}
```
