# POST Reply to a Pull Request Review Comment

## Endpoint

```
POST /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies
```

Full URL: `https://api.github.com/repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies`

This creates a reply to an existing review comment in a pull request thread. The reply is threaded under the original comment identified by `comment_id`.

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
| `repo` | string | yes | Repository name without `.git` extension |
| `pull_number` | integer | yes | The pull request number |
| `comment_id` | integer | yes | The ID of the review comment to reply to |

---

## Request Body

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `body` | string | yes | The text of the reply comment (Markdown) |

Example:
```json
{
  "body": "Thanks for the feedback, I've addressed this in the latest commit."
}
```

---

## Response

### Status Codes

| Code | Meaning |
|------|---------|
| 201 Created | Reply created successfully |
| 404 Not Found | PR, comment, or repo not found |

### Response Body (201 Created)

```json
{
  "url": "https://api.github.com/repos/octocat/Hello-World/pulls/comments/10",
  "pull_request_review_id": 42,
  "id": 10,
  "node_id": "MDI0OlB1bGxSZXF1ZXN0UmV2aWV3Q29tbWVudDEw",
  "diff_hunk": "@@ -16,33 +16,40 @@ public class Connection : IConnection...",
  "path": "src/connection.ml",
  "position": 1,
  "original_position": 4,
  "commit_id": "ecdd80bb57125d7ba9641ffde853670f70daa1b2",
  "original_commit_id": "9c48853fa3dc5c1c3d6f1f1cd1f2491aada68743",
  "in_reply_to_id": 8,
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
  "body": "Thanks for the feedback, I've addressed this in the latest commit.",
  "created_at": "2011-04-14T16:00:49Z",
  "updated_at": "2011-04-14T16:00:49Z",
  "html_url": "https://github.com/octocat/Hello-World/pull/1#discussion_r10",
  "pull_request_url": "https://api.github.com/repos/octocat/Hello-World/pulls/1",
  "author_association": "COLLABORATOR",
  "_links": {
    "self": {
      "href": "https://api.github.com/repos/octocat/Hello-World/pulls/comments/10"
    },
    "html": {
      "href": "https://github.com/octocat/Hello-World/pull/1#discussion_r10"
    },
    "pull_request": {
      "href": "https://api.github.com/repos/octocat/Hello-World/pulls/1"
    }
  },
  "start_line": null,
  "original_start_line": null,
  "start_side": null,
  "line": 1,
  "original_line": 1,
  "side": "RIGHT"
}
```

### Response Field Reference

| Field | Type | Description |
|-------|------|-------------|
| `id` | integer | Unique review comment ID |
| `node_id` | string | GraphQL global node ID |
| `url` | string (URI) | REST API URL for this comment |
| `html_url` | string (URI) | Web URL for the comment (with fragment anchor) |
| `pull_request_review_id` | integer or null | ID of the review this comment belongs to |
| `diff_hunk` | string | The relevant lines from the diff |
| `path` | string | File path the comment is on |
| `position` | integer | Line index in the diff (may differ from `line`) |
| `original_position` | integer | Position in the original diff |
| `commit_id` | string | SHA of the commit the comment was placed on |
| `original_commit_id` | string | SHA of the original commit |
| `in_reply_to_id` | integer | ID of the comment being replied to |
| `user` | object | Author (same shape as in issue comments) |
| `body` | string | Comment text (Markdown) |
| `created_at` | string (ISO 8601) | Creation timestamp |
| `updated_at` | string (ISO 8601) | Last edit timestamp |
| `pull_request_url` | string (URI) | REST API URL for the associated PR |
| `author_association` | string (enum) | Commenter's relationship to the repo |
| `_links` | object | Hypermedia links (self, html, pull_request) |
| `start_line` | integer or null | For multi-line comments: starting line |
| `original_start_line` | integer or null | Original starting line |
| `start_side` | string or null | `"LEFT"` or `"RIGHT"` for start of multi-line |
| `line` | integer | Line number in the file the comment applies to |
| `original_line` | integer | Original line number |
| `side` | string | `"LEFT"` (deleted) or `"RIGHT"` (added/context) |

---

## Authentication

Same as issue comments: `Authorization: Bearer <token>`.

Required permission: **Pull requests write** on the target repository.

---

## curl Example

```bash
curl -L \
  -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ghp_YOUR_TOKEN" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  -H "Content-Type: application/json" \
  https://api.github.com/repos/OWNER/REPO/pulls/42/comments/1234/replies \
  -d '{"body":"Addressed in the latest push."}'
```

---

## Notes for OCaml Parser

Key distinction from issue comments: the response is a **pull_request_review_comment** object, not an issue comment object. Key distinguishing fields:
- `in_reply_to_id` is present (the parent comment ID)
- `diff_hunk` and `path` are always present (line-level context)
- `side` is `"LEFT"` or `"RIGHT"` (which side of the diff)
- No `issue_url` field; instead has `pull_request_url`

For the reply case, `position` reflects the position of the *parent* comment's location in the diff.
