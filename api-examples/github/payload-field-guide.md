# GitHub Webhook Payload Field Guide

Reference for OCaml JSON parser implementation. All paths use dot notation.
Array indexing shown where the field is inside an array element.

Sources:
- https://docs.github.com/en/webhooks/webhook-events-and-payloads
- https://github.com/octokit/webhooks (payload-examples/api.github.com/)

---

## HTTP Headers (all event types)

| Header | Description |
|--------|-------------|
| `X-GitHub-Event` | Event type name (e.g., `pull_request`, `issue_comment`) |
| `X-GitHub-Delivery` | Unique GUID for this delivery |
| `X-Hub-Signature-256` | `sha256=HEXDIGEST` — HMAC-SHA256 of raw body using webhook secret |
| `X-Hub-Signature` | `sha1=HEXDIGEST` — legacy SHA-1 signature (deprecated, avoid) |
| `X-GitHub-Hook-ID` | Webhook ID integer |
| `X-GitHub-Hook-Installation-Target-ID` | Repository/org/app ID |
| `X-GitHub-Hook-Installation-Target-Type` | `repository`, `organization`, or `app` |
| `Content-Type` | `application/json` |

---

## Event: `pull_request`

**X-GitHub-Event header value:** `pull_request`

**File:** `payload-pull_request-opened.json`, `payload-pull_request-edited.json`, `payload-pull_request-synchronize.json`

### Actions documented here

| Action | Trigger |
|--------|---------|
| `opened` | PR was opened |
| `edited` | PR title or body was edited |
| `synchronize` | New commits pushed to the PR branch |
| `reopened` | Closed PR was reopened |

### Key field paths

| Purpose | JSON path |
|---------|-----------|
| Action | `.action` — string: `"opened"`, `"edited"`, `"synchronize"`, `"reopened"` |
| PR number | `.number` or `.pull_request.number` — integer |
| PR title | `.pull_request.title` — string |
| PR body text (where /clawq might appear) | `.pull_request.body` — string or null |
| PR HTML URL | `.pull_request.html_url` — string |
| PR author username | `.pull_request.user.login` — string |
| Repository name | `.repository.name` — string (e.g., `"Hello-World"`) |
| Repository owner username | `.repository.owner.login` — string (e.g., `"Codertocat"`) |
| Repository full name | `.repository.full_name` — string (e.g., `"Codertocat/Hello-World"`) |
| Sender username | `.sender.login` — string (user who triggered the event) |
| Head branch ref | `.pull_request.head.ref` — string |
| Head SHA | `.pull_request.head.sha` — string |
| Base branch ref | `.pull_request.base.ref` — string |
| PR state | `.pull_request.state` — `"open"` or `"closed"` |
| PR draft status | `.pull_request.draft` — boolean |

### `edited` action only

When action is `edited`, the payload also includes:

```json
"changes": {
  "title": { "from": "<old title>" },
  "body": { "from": "<old body>" }
}
```

Only the fields that changed are present. If only the body changed, only `changes.body` appears; if only the title changed, only `changes.title` appears.

### `synchronize` action only

When action is `synchronize`, the payload also includes:

| Purpose | JSON path |
|---------|-----------|
| Previous head SHA | `.before` — string |
| New head SHA | `.after` — string |

---

## Event: `issue_comment`

**X-GitHub-Event header value:** `issue_comment`

**File:** `payload-issue_comment-created.json`

### Actions

| Action | Trigger |
|--------|---------|
| `created` | Comment was posted |
| `edited` | Comment was edited |
| `deleted` | Comment was deleted |

### Key field paths

| Purpose | JSON path |
|---------|-----------|
| Action | `.action` — string: `"created"`, `"edited"`, `"deleted"` |
| Issue/PR number | `.issue.number` — integer |
| Issue/PR title | `.issue.title` — string |
| Issue/PR HTML URL | `.issue.html_url` — string |
| Issue/PR body | `.issue.body` — string or null |
| Issue/PR author | `.issue.user.login` — string |
| Comment body text (where /clawq might appear) | `.comment.body` — string |
| Comment HTML URL | `.comment.html_url` — string |
| Comment author username | `.comment.user.login` — string |
| Comment ID | `.comment.id` — integer |
| Repository name | `.repository.name` — string |
| Repository owner username | `.repository.owner.login` — string |
| Repository full name | `.repository.full_name` — string |
| Sender username | `.sender.login` — string |

### Detecting if the issue_comment is on a PR (vs. a plain issue)

If `.issue.pull_request` is present (non-null object), the comment is on a pull request.
The PR URL is at `.issue.pull_request.url`.

---

## Event: `pull_request_review_comment`

**X-GitHub-Event header value:** `pull_request_review_comment`

**File:** `payload-pull_request_review_comment-created.json`

These are inline code review comments on specific diff lines, not general PR comments.

### Actions

| Action | Trigger |
|--------|---------|
| `created` | Review comment was posted |
| `edited` | Review comment was edited |
| `deleted` | Review comment was deleted |

### Key field paths

| Purpose | JSON path |
|---------|-----------|
| Action | `.action` — string |
| Comment body text (where /clawq might appear) | `.comment.body` — string |
| Comment HTML URL | `.comment.html_url` — string |
| Comment author username | `.comment.user.login` — string |
| Comment ID | `.comment.id` — integer |
| PR number | `.pull_request.number` — integer |
| PR title | `.pull_request.title` — string |
| PR HTML URL | `.pull_request.html_url` — string |
| PR author | `.pull_request.user.login` — string |
| File path the comment is on | `.comment.path` — string |
| Diff hunk context | `.comment.diff_hunk` — string |
| Commit SHA the comment is on | `.comment.commit_id` — string |
| Which side of the diff | `.comment.side` — `"LEFT"` or `"RIGHT"` |
| Line number in the file | `.comment.line` — integer or null |
| Start line (for multi-line comment) | `.comment.start_line` — integer or null |
| Review ID this comment belongs to | `.comment.pull_request_review_id` — integer |
| Repository name | `.repository.name` — string |
| Repository owner username | `.repository.owner.login` — string |
| Repository full name | `.repository.full_name` — string |
| Sender username | `.sender.login` — string |

---

## Event: `pull_request_review`

**X-GitHub-Event header value:** `pull_request_review`

**File:** `payload-pull_request_review-submitted.json`

A review is the overall review submission (approved, changes requested, or commented).

### Actions

| Action | Trigger |
|--------|---------|
| `submitted` | Review was submitted |
| `edited` | Review body was edited |
| `dismissed` | Review was dismissed |

### Key field paths

| Purpose | JSON path |
|---------|-----------|
| Action | `.action` — string: `"submitted"`, `"edited"`, `"dismissed"` |
| Review body text (where /clawq might appear) | `.review.body` — string or **null** (can be null if no body text, only inline comments) |
| Review HTML URL | `.review.html_url` — string |
| Review author username | `.review.user.login` — string |
| Review state | `.review.state` — `"approved"`, `"changes_requested"`, or `"commented"` |
| Review submission time | `.review.submitted_at` — ISO 8601 string |
| Commit SHA the review is on | `.review.commit_id` — string |
| Review ID | `.review.id` — integer |
| PR number | `.pull_request.number` — integer |
| PR title | `.pull_request.title` — string |
| PR HTML URL | `.pull_request.html_url` — string |
| PR author | `.pull_request.user.login` — string |
| Repository name | `.repository.name` — string |
| Repository owner username | `.repository.owner.login` — string |
| Repository full name | `.repository.full_name` — string |
| Sender username | `.sender.login` — string |

**Note:** When `review.state` is `"commented"` and inline review comments were added (via `pull_request_review_comment` events), `review.body` may be null. Always check for null before reading the body.

---

## Common patterns for /clawq command detection

### Where the /clawq command can appear

| Event | Body field to check |
|-------|---------------------|
| `pull_request` (opened/edited) | `.pull_request.body` |
| `issue_comment` (created) | `.comment.body` |
| `pull_request_review_comment` (created) | `.comment.body` |
| `pull_request_review` (submitted) | `.review.body` (may be null) |

### Extracting repo owner and name

For all events:
- Owner: `.repository.owner.login`
- Repo name: `.repository.name`
- Full name (owner/repo): `.repository.full_name`

### Extracting the PR number

| Event | PR number field |
|-------|----------------|
| `pull_request` | `.number` (also `.pull_request.number`) |
| `issue_comment` on PR | `.issue.number` |
| `pull_request_review_comment` | `.pull_request.number` |
| `pull_request_review` | `.pull_request.number` |

---

## Optional `installation` field

When a webhook is delivered via a GitHub App installation, the payload includes:

```json
"installation": {
  "id": 1,
  "node_id": "MDIzOkludGVncmF0aW9uSW5zdGFsbGF0aW9uMQ=="
}
```

This field is absent for repository/organization webhooks configured directly (not through a GitHub App). Always treat it as optional.

---

## Payload size note

GitHub caps webhook payloads at 25 MB. If an event exceeds this size, the webhook delivery is skipped silently.
