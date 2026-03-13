# GitHub REST API Reference

Notes captured from the GitHub REST API documentation. Used by `src/github_api.ml` and `src/github.ml`.

## Authentication

All requests use a Personal Access Token (PAT) via Bearer auth:

```
Authorization: Bearer ghp_...
Accept: application/vnd.github+json
X-GitHub-Api-Version: 2022-11-28
```

See `github_api.ml:auth_headers` for the implementation.

## Endpoints Used

### Post an Issue/PR Comment

Creates a comment on an issue or pull request (issues and PRs share the same comment namespace).

```
POST /repos/{owner}/{repo}/issues/{issue_number}/comments
```

Request body:
```json
{ "body": "Comment text" }
```

Response: `201 Created` with the created comment object (includes `id` field).

Used by: `post_comment`, `post_comment_returning_id`

Reference: https://docs.github.com/en/rest/issues/comments#create-an-issue-comment

### Edit an Issue Comment

Updates an existing issue comment by ID. Used to replace placeholder comments with final responses.

```
PATCH /repos/{owner}/{repo}/issues/comments/{comment_id}
```

Request body:
```json
{ "body": "Updated comment text" }
```

Response: `200 OK`

Used by: `edit_comment`

Reference: https://docs.github.com/en/rest/issues/comments#update-an-issue-comment

### Reply to a Pull Request Review Comment

Creates an in-thread reply to an existing review comment. The reply appears nested under the original review comment.

```
POST /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies
```

Request body:
```json
{ "body": "Reply text" }
```

Response: `201 Created`

Used by: `reply_to_review_comment`

Reference: https://docs.github.com/en/rest/pulls/comments#create-a-reply-for-a-review-comment

### Add a Reaction to an Issue Comment

Adds a reaction (e.g. "eyes") to an issue comment. Used for acknowledgment.

```
POST /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions
```

Request body:
```json
{ "content": "eyes" }
```

Valid reaction content values: `+1`, `-1`, `laugh`, `confused`, `heart`, `hooray`, `rocket`, `eyes`

Response: `200 OK` (if reaction already exists) or `201 Created`

Used by: `add_reaction` with `~comment_type:`Issue``

Reference: https://docs.github.com/en/rest/reactions/reactions#create-reaction-for-an-issue-comment

### Add a Reaction to a Pull Request Review Comment

Adds a reaction to a review comment. Same payload format as issue comment reactions.

```
POST /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions
```

Request body:
```json
{ "content": "eyes" }
```

Response: `200 OK` or `201 Created`

Used by: `add_reaction` with `~comment_type:`Review``

Reference: https://docs.github.com/en/rest/reactions/reactions#create-reaction-for-a-pull-request-review-comment

### List Pull Request Files

Returns the list of files changed in a pull request. Paginated, up to 100 per page.

```
GET /repos/{owner}/{repo}/pulls/{pull_number}/files?per_page=100&page={page}
```

Response: `200 OK` with array of file objects:
```json
[
  {
    "filename": "src/foo.ml",
    "status": "modified",
    "additions": 10,
    "deletions": 3
  }
]
```

Used by: `get_pr_files` (fetches up to 3 pages / 300 files)

Reference: https://docs.github.com/en/rest/pulls/pulls#list-pull-requests-files

## Webhook Verification

GitHub signs webhook payloads with HMAC-SHA256 using the configured webhook secret. The signature is sent in the `X-Hub-Signature-256` header as `sha256=<hex_digest>`.

Verification: compute `HMAC-SHA256(secret, raw_body)` and compare the hex digest with constant-time comparison.

See `github_webhook.ml:verify_signature` for the implementation.

Reference: https://docs.github.com/en/webhooks/using-webhooks/validating-webhook-deliveries

## Webhook Headers

| Header | Purpose |
|--------|---------|
| `X-GitHub-Event` | Event type (e.g. `issue_comment`, `pull_request`, `pull_request_review_comment`) |
| `X-Hub-Signature-256` | HMAC-SHA256 signature for payload verification |
| `X-GitHub-Delivery` | Unique delivery ID (used for deduplication) |

## Comment Lifecycle Pattern

The clawq GitHub integration follows this lifecycle for `/clawq` command processing:

1. **Acknowledge** --- add an "eyes" reaction to the triggering comment via the reactions API.
2. **Placeholder** --- post a placeholder comment with a spinner (for issue/PR comments; skipped for review comments which don't support editing in the same way).
3. **Process** --- run the agent turn asynchronously.
4. **Finalize** --- edit the placeholder comment with the final response (or post a new comment/reply if no placeholder exists).
5. **Notify** --- channel notifier registration ensures autonomous/deferred agent responses also reach the correct GitHub thread.

Bot self-loop protection: all bot replies include a `<!-- clawq-reply -->` HTML comment marker. Incoming comments containing this marker are silently ignored.

Delivery deduplication: the `X-GitHub-Delivery` header is tracked in an in-memory LRU set (capacity 500). Duplicate deliveries are rejected before processing.
