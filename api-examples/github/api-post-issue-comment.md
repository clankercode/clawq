# POST Issue or PR Comment

## Endpoint

```
POST /repos/{owner}/{repo}/issues/{issue_number}/comments
```

Full URL: `https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments`

Note: GitHub uses a unified issues API for both issues and pull requests. A PR is an issue with extra metadata. This endpoint works identically for both — use the PR number as `issue_number`.

---

## Required Headers

| Header | Value |
|--------|-------|
| `Authorization` | `Bearer <YOUR-TOKEN>` |
| `Accept` | `application/vnd.github+json` |
| `X-GitHub-Api-Version` | `2022-11-28` |
| `Content-Type` | `application/json` |

---

## Path Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `owner` | string | yes | Repository owner login (case-insensitive) |
| `repo` | string | yes | Repository name without `.git` extension (case-insensitive) |
| `issue_number` | integer | yes | Issue or PR number |

---

## Request Body

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `body` | string | yes | The Markdown content of the comment |

Example:
```json
{
  "body": "This is a comment on the issue or PR."
}
```

---

## Response

### Status Codes

| Code | Meaning |
|------|---------|
| 201 Created | Comment created successfully |
| 403 Forbidden | Authenticated user lacks write permission |
| 404 Not Found | Repository or issue not found |
| 410 Gone | Issue is locked or deleted |
| 422 Unprocessable Entity | Validation error or spam detection triggered |

### Response Body (201 Created)

```json
{
  "id": 1,
  "node_id": "MDEyOklzc3VlQ29tbWVudDE=",
  "url": "https://api.github.com/repos/octocat/Hello-World/issues/comments/1",
  "html_url": "https://github.com/octocat/Hello-World/issues/1347#issuecomment-1",
  "body": "Me too",
  "user": {
    "login": "octocat",
    "id": 1,
    "node_id": "MDQ6VXNlcjE=",
    "avatar_url": "https://github.com/images/error/octocat_happy.gif",
    "gravatar_id": "",
    "url": "https://api.github.com/users/octocat",
    "html_url": "https://github.com/octocat",
    "followers_url": "https://api.github.com/users/octocat/followers",
    "following_url": "https://api.github.com/users/octocat/following{/other_user}",
    "gists_url": "https://api.github.com/users/octocat/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/octocat/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/octocat/subscriptions",
    "organizations_url": "https://api.github.com/users/octocat/orgs",
    "repos_url": "https://api.github.com/users/octocat/repos",
    "events_url": "https://api.github.com/users/octocat/events{/privacy}",
    "received_events_url": "https://api.github.com/users/octocat/received_events",
    "type": "User",
    "site_admin": false
  },
  "created_at": "2011-04-14T16:00:49Z",
  "updated_at": "2011-04-14T16:00:49Z",
  "issue_url": "https://api.github.com/repos/octocat/Hello-World/issues/1347",
  "author_association": "COLLABORATOR",
  "performed_via_github_app": null,
  "reactions": {
    "url": "https://api.github.com/repos/octocat/Hello-World/issues/comments/1/reactions",
    "total_count": 0,
    "+1": 0,
    "-1": 0,
    "laugh": 0,
    "hooray": 0,
    "confused": 0,
    "heart": 0,
    "rocket": 0,
    "eyes": 0
  }
}
```

### Response Field Reference

| Field | Type | Description |
|-------|------|-------------|
| `id` | integer | Unique comment ID (used to reference or edit the comment) |
| `node_id` | string | GraphQL global node ID |
| `url` | string (URI) | REST API URL for this comment |
| `html_url` | string (URI) | Web URL for the comment on GitHub |
| `body` | string | Comment text (Markdown) |
| `user` | object or null | Author of the comment (see user object below) |
| `created_at` | string (ISO 8601) | Timestamp when the comment was created |
| `updated_at` | string (ISO 8601) | Timestamp of last edit |
| `issue_url` | string (URI) | REST API URL for the associated issue or PR |
| `author_association` | string (enum) | Commenter's relationship to the repo (see below) |
| `performed_via_github_app` | object or null | If posted via a GitHub App, app details; otherwise null |
| `reactions` | object | Reaction counts (present when using `application/vnd.github+json`) |
| `pin` | object or null | Pin metadata; null if not pinned |

#### user object fields

| Field | Type | Description |
|-------|------|-------------|
| `login` | string | GitHub username |
| `id` | integer | User's numeric ID |
| `node_id` | string | GraphQL node ID |
| `avatar_url` | string (URI) | Profile image URL |
| `gravatar_id` | string | Gravatar ID (usually empty string) |
| `url` | string (URI) | REST API URL for the user |
| `html_url` | string (URI) | Web profile URL |
| `followers_url` | string (URI) | |
| `following_url` | string (URI template) | |
| `gists_url` | string (URI template) | |
| `starred_url` | string (URI template) | |
| `subscriptions_url` | string (URI) | |
| `organizations_url` | string (URI) | |
| `repos_url` | string (URI) | |
| `events_url` | string (URI template) | |
| `received_events_url` | string (URI) | |
| `type` | string | `"User"`, `"Bot"`, or `"Organization"` |
| `site_admin` | boolean | Whether the user is a GitHub site admin |

#### author_association enum values

| Value | Meaning |
|-------|---------|
| `COLLABORATOR` | Has been invited to collaborate |
| `CONTRIBUTOR` | Previously committed to the repo |
| `FIRST_TIMER` | First contribution to GitHub overall |
| `FIRST_TIME_CONTRIBUTOR` | First contribution to this repo |
| `MANNEQUIN` | Placeholder user from importer |
| `MEMBER` | Member of the org that owns the repo |
| `NONE` | No special relationship |
| `OWNER` | Owner of the repo |

---

## Authentication: Using a Personal Access Token (PAT)

Include the PAT in the `Authorization` header as a Bearer token:

```
Authorization: Bearer ghp_yourClassicTokenHere
```

or for fine-grained PATs:

```
Authorization: Bearer github_pat_yourFineGrainedTokenHere
```

Token formats:
- Classic PAT: starts with `ghp_`
- Fine-grained PAT: starts with `github_pat_`

Required permission: **Issues write** (or **Pull requests write**) on the target repository.

Both token types support this endpoint. Fine-grained tokens are preferred for security (scoped to specific repos and permissions).

---

## curl Example

```bash
curl -L \
  -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ghp_YOUR_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  -H "Content-Type: application/json" \
  https://api.github.com/repos/OWNER/REPO/issues/42/comments \
  -d '{"body":"This looks good to me!"}'
```

---

## Notes for OCaml Parser

The minimal fields needed for a successful POST are confirmed present in the 201 response:
- `id` (int): use to reference the comment in subsequent requests
- `html_url` (string): the canonical web link to show users
- `body` (string): echoed back
- `created_at` (string): ISO 8601 datetime
- `user.login` (string): who posted it

The `reactions` object may be absent if a legacy `Accept` header is used. The `performed_via_github_app` field is `null` for PAT or user-token authenticated requests.
