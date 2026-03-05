# GitHub API Authentication Models: PAT vs GitHub App

## 1. Personal Access Token (PAT)

The simplest authentication method. A PAT represents a GitHub user and grants the token-holder the same access as that user (limited by the token's scopes/permissions).

### Token Formats

| Type | Prefix | Notes |
|------|--------|-------|
| Classic PAT | `ghp_` | Broad scopes, works everywhere, less secure |
| Fine-grained PAT | `github_pat_` | Per-repo permission grants, recommended |

### Authorization Header

```
Authorization: Bearer ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

or for fine-grained:

```
Authorization: Bearer github_pat_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Both use the same `Authorization: Bearer <token>` format. GitHub also accepts `Authorization: token <token>` (legacy form) but `Bearer` is preferred and required for JWTs.

### Required Permissions for Our Use Cases

| Operation | Required Permission |
|-----------|-------------------|
| POST issue comment | Issues: Write |
| POST PR review comment reply | Pull requests: Write |
| GET PR files | Pull requests: Read (or Contents: Read) |

### Characteristics

- Token is static; does not expire unless revoked or set to expire at creation.
- Acts as the user who created the token; attribution shows that user's name.
- Rate limit: 5,000 requests/hour under the user's quota.
- Simplest to implement: one environment variable, one header.

### Example curl

```bash
curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ghp_YOUR_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/OWNER/REPO/issues/1/comments \
  -X POST \
  -d '{"body": "Hello!"}'
```

---

## 2. GitHub App Authentication

GitHub Apps are first-class principals (not users). They have their own identity, icon, and rate limits. Authentication is a two-step flow.

### Flow Overview

```
[App private key + App ID]
        |
        v
  Generate JWT (RS256, 10-min TTL)
        |
        v
  POST /app/installations/{installation_id}/access_tokens
  Authorization: Bearer <JWT>
        |
        v
  Receive installation access token (1-hour TTL)
        |
        v
  Use token for API calls:
  Authorization: Bearer <installation_token>
```

### Step 1: Generate a JWT

The JWT is signed with the app's RSA private key using the RS256 algorithm.

JWT payload:
```json
{
  "iat": <now_unix - 60>,
  "exp": <now_unix + 600>,
  "iss": "YOUR_GITHUB_APP_CLIENT_ID"
}
```

- `iat`: issued-at, set 60 seconds in the past to tolerate clock skew between client and GitHub servers.
- `exp`: expiry, maximum 10 minutes (600 seconds) from now.
- `iss`: the GitHub App's **client ID** (string), not the numeric app ID (though the numeric app ID was previously used and may still be accepted).

The private key must be in PEM format (RSA, typically 2048-bit).

### Step 2: Exchange JWT for Installation Token

```
POST https://api.github.com/app/installations/{installation_id}/access_tokens
Authorization: Bearer <JWT>
Accept: application/vnd.github+json
X-GitHub-Api-Version: 2022-11-28
```

Optional request body to restrict token scope:
```json
{
  "repositories": ["my-repo"],
  "permissions": {
    "issues": "write",
    "pull_requests": "write"
  }
}
```

Response:
```json
{
  "token": "ghs_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "expires_at": "2024-01-15T12:00:00Z",
  "permissions": {
    "issues": "write",
    "pull_requests": "write"
  },
  "repository_selection": "selected",
  "repositories": [...]
}
```

- `token`: installation access token; prefix `ghs_`
- `expires_at`: ISO 8601 timestamp; always 1 hour from creation
- Token must be refreshed before expiry

### Step 3: Use the Installation Token

```
Authorization: Bearer ghs_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Identical header format to PAT — just a different token value.

### GitHub App Characteristics

| Property | Value |
|----------|-------|
| Token TTL | 1 hour (must refresh) |
| Attribution | "YourAppName[bot]" user in GitHub UI |
| Rate limit | Higher ceiling; scales with installation (up to 15,000/hr for Enterprise orgs) |
| Finding installation_id | Via `GET /app/installations` or from webhook payloads |

---

## 3. Key Differences at a Glance

| Property | PAT | GitHub App Installation Token |
|----------|-----|-------------------------------|
| Token prefix | `ghp_` or `github_pat_` | `ghs_` |
| Authorization header | `Bearer <pat>` | `Bearer <installation_token>` |
| Expiry | None (or user-set date) | 1 hour |
| Attributed to | The user who created the PAT | The GitHub App (shows as `[bot]`) |
| Requires rotation logic | No | Yes — must refresh hourly |
| Rate limit | 5,000/hr (user quota) | Higher (app quota, shared across installations) |
| Setup complexity | Minimal (one token) | Moderate (App registration, private key, installation ID) |
| Scope | User-level | Installation-level (per-repo or all-repos of an org) |

---

## 4. Abstraction for Switching Between Auth Methods

Both methods end up with the same thing: a bearer token string and (optionally) an expiry time. The abstraction boundary is at token acquisition, not at request time.

### Proposed OCaml Type

```ocaml
type token_source =
  | Static_pat of string
  (* A fixed token string (classic or fine-grained PAT). Never expires. *)

  | Github_app of {
      app_id        : string;
      private_key   : string;   (* PEM-encoded RSA private key *)
      installation_id : int;
      (* Runtime state below — managed by the token cache *)
      mutable cached_token : string option;
      mutable token_expires_at : float option;  (* Unix timestamp *)
    }

type auth_state = {
  source : token_source;
}

val get_token : auth_state -> string Lwt.t
(** Returns the current bearer token, refreshing if needed (for App auth). *)
```

### get_token Logic

```
match source with
| Static_pat tok -> Lwt.return tok
| Github_app app ->
    let now = Unix.gettimeofday () in
    match app.cached_token, app.token_expires_at with
    | Some tok, Some exp when exp -. now > 60.0 ->
        (* Token is still valid with >60s buffer *)
        Lwt.return tok
    | _ ->
        (* Token missing or expiring soon — refresh *)
        let* new_tok = fetch_installation_token app in
        app.cached_token <- Some new_tok.token;
        app.token_expires_at <- Some (parse_iso8601 new_tok.expires_at);
        Lwt.return new_tok.token
```

### Request Helper

```ocaml
val make_github_request :
  auth:auth_state ->
  meth:[ `GET | `POST | `PATCH | `DELETE ] ->
  url:string ->
  ?body:string ->
  unit ->
  (Cohttp.Response.t * string) Lwt.t
```

This helper calls `get_token auth` first, then sets the `Authorization: Bearer <token>` header, plus the standard `Accept` and `X-GitHub-Api-Version` headers. The rest of the codebase never touches token management directly.

### Configuration

```json
{
  "github": {
    "auth": {
      "type": "pat",
      "token": "ghp_..."
    }
  }
}
```

or:

```json
{
  "github": {
    "auth": {
      "type": "github_app",
      "app_id": "123456",
      "private_key_path": "/etc/clawq/github-app.pem",
      "installation_id": 789012
    }
  }
}
```

Switching between auth methods is a config change; no call-site code changes needed.
