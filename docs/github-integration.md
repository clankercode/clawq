# GitHub Integration Guide

Clawq integrates with GitHub through a GitHub App (or PAT fallback) to deliver
pull request notifications, trigger automated review runs, and maintain
bidirectional audit trails between GitHub and Clawq rooms.

## Table of Contents

1. [GitHub App Setup](#1-github-app-setup)
2. [Authentication](#2-authentication)
3. [Repo Grants](#3-repo-grants)
4. [PR Subscriptions](#4-pr-subscriptions)
5. [Event Dispatch](#5-event-dispatch)
6. [CI/Review Reporting](#6-cireview-reporting)
7. [Review Runs](#7-review-runs)
8. [Backlinks](#8-backlinks)
9. [Troubleshooting](#9-troubleshooting)

---

## 1. GitHub App Setup

### Creating the App

1. Go to **GitHub > Settings > Developer settings > GitHub Apps > New GitHub App**.
2. Set the **Webhook URL** to your Clawq instance's webhook endpoint
   (e.g. `https://your-host/github/webhook`).
3. Set a **Webhook secret** and record it -- you will need it for the config.

### Required Permissions

| Category | Permission | Level |
|----------|-----------|-------|
| Pull requests | Read & Write | For PR events and posting comments |
| Checks | Read | For check run/suite status events |
| Contents | Read | For reading repo files during reviews |
| Issues | Read & Write | For issue comment events on PRs |
| Metadata | Read | Required for all GitHub Apps |

### Webhook Events

Subscribe to the following events:

- **Pull request** -- open, close, synchronize, review, label changes
- **Issue comment** -- comments on PRs
- **Pull request review** -- reviews and review comments
- **Check run** -- CI status updates
- **Check suite** -- CI suite status updates
- **Workflow run** -- GitHub Actions workflow status

### Installation

After creating the app, install it on target organizations/repositories. Each
installation generates an `installation_id` that you must add to the Clawq
config.

---

## 2. Authentication

Clawq supports two authentication methods:

### GitHub App (Recommended)

The GitHub App flow uses JWT-based authentication with automatic token caching:

1. A PEM private key (RSA, PKCS#8 or PKCS#1) is loaded from disk.
2. A short-lived JWT (RS256, max 10 minutes) is generated and signed with the
   private key.
3. The JWT is exchanged for an installation access token via the GitHub API.
4. Tokens are cached for ~50 minutes (tokens expire after 60 min).

All token values are automatically redacted in log output.

#### Config

```yaml
github:
  auth:
    type: app
    app_id: 123456
    private_key_path: /path/to/private-key.pem
    webhook_secret: your-webhook-secret
    installations:
      - installation_id: 12345678
        repos: []  # empty = all repos in the installation
      - installation_id: 87654321
        repos:
          - owner/repo-1
          - owner/repo-2
```

#### Environment Variable

For GitHub Enterprise Server, set the API base URL:

```bash
export CLAWQ_GITHUB_API_BASE="https://github.example.com/api/v3"
```

### PAT Fallback

For simpler setups, use a Personal Access Token:

```yaml
github:
  auth:
    type: pat
    token: ghp_xxxxxxxxxxxxxxxxxxxx
```

PAT auth skips the JWT and installation token flow entirely. It is not
recommended for production because it uses a single identity for all repos.

### Credential Lease API

For managed credential environments, set `auth_credential_handle` in the GitHub
config. When set, GitHub API calls resolve credentials through the credential
lease API using this handle ID, scoped by the access snapshot. When omitted,
the raw `auth` field is used directly.

---

## 3. Repo Grants

### Per-Repo Configuration

Each repository can be configured individually with its own webhook secret,
agent name, and user allowlist:

```yaml
github:
  repos:
    - name: owner/my-repo
      webhook_secret: per-repo-secret   # overrides app-level secret
      webhook_path: /github/webhook
      agent_name: code-reviewer          # optional: agent template for reviews
      allow_users:
        - alice
        - bob
      react_to:
        - opened
        - synchronize
      include_pr_files: true             # include changed file list in context
```

### Capability Model

| Field | Description |
|-------|-------------|
| `name` | Full repository name (`owner/repo`) |
| `webhook_secret` | Per-repo webhook secret (overrides app-level) |
| `webhook_path` | Webhook endpoint path for this repo |
| `agent_name` | Agent template name for review runs |
| `allow_users` | GitHub usernames allowed to interact with Clawq via this repo |
| `react_to` | List of PR actions that trigger notifications (empty = all) |
| `include_pr_files` | Whether to include changed files in review prompts |

### Installation Scoping

Installation-level repo grants control which repos a GitHub App installation can
access:

```yaml
installations:
  - installation_id: 12345678
    repos: []  # empty = all repos in the installation
  - installation_id: 87654321
    repos:
      - owner/repo-1
      - owner/repo-2
```

The `verify_installation` function checks that a webhook's `installation_id`
matches a configured installation and that the repo is within scope.

---

## 4. PR Subscriptions

PR subscriptions link a specific GitHub pull request to a Clawq room. When
webhook events match a subscription, notifications are delivered to the
subscribed room.

### Subscription Model

Each subscription is uniquely identified by the composite key
`(room_id, repo, pr_number)`. Creating a subscription with an existing key
upserts (updates the profile and preferences).

| Field | Type | Description |
|-------|------|-------------|
| `id` | integer | Auto-incremented primary key |
| `room_id` | string | Target room for notifications |
| `repo` | string | Full repo name (`owner/repo`) |
| `pr_number` | integer | Pull request number |
| `profile_id` | integer | Room profile for agent behavior |
| `enabled` | boolean | Whether the subscription is active |
| `notification_preferences` | object | Per-event-type toggle flags |

### Notification Preferences

Each subscription has fine-grained preference toggles for event types:

| Preference | Default | Covers |
|-----------|---------|--------|
| `on_open` | `true` | `opened`, `reopened` |
| `on_close` | `true` | `closed` |
| `on_comment` | `true` | `comment`, `issue_comment`, `review_comment` |
| `on_review` | `true` | `review`, `review_requested` |
| `on_status` | `true` | `status`, `check_run`, `check_suite` |
| `on_merge` | `true` | `merged` |

Unknown event types default to `true` (always notify).

### Admin CLI

All subscription commands require the `CLAWQ_ADMIN=1` environment variable.

```bash
# Set admin mode
export CLAWQ_ADMIN=1

# List all subscriptions
clawq subscriptions list

# List subscriptions for a specific room
clawq subscriptions list --room my-room

# List subscriptions for a specific repo
clawq subscriptions list --repo owner/repo

# Show subscription details
clawq subscriptions show 42

# Add a subscription with default preferences
clawq subscriptions add my-room owner/repo 123

# Add with custom profile and notification preferences
clawq subscriptions add my-room owner/repo 123 \
  --profile code-reviewer \
  --on-open true \
  --on-close false \
  --on-comment true \
  --on-review false \
  --on-status true \
  --on-merge false

# Disable/enable a subscription (by ID)
clawq subscriptions disable 42
clawq subscriptions enable 42

# Remove by ID
clawq subscriptions remove 42

# Remove by room/repo/PR
clawq subscriptions remove my-room owner/repo 123
```

### Bulk Operations

```bash
# Delete all subscriptions for a room
# (programmatic: Github_pr_subscriptions.delete_by_room)

# Delete all subscriptions for a repo
# (programmatic: Github_pr_subscriptions.delete_by_repo)

# Get total subscription count
# (programmatic: Github_pr_subscriptions.count)
```

---

## 5. Event Dispatch

### How Webhook Events Reach Rooms

1. GitHub sends a webhook event to Clawq's webhook endpoint.
2. The event is parsed into a `parsed_event` variant type.
3. The dispatch module extracts `repo` and `pr_number` from the event.
4. All subscriptions matching `(repo, pr_number)` are loaded.
5. Each subscription is checked against notification preferences and policy
   gates.
6. If allowed, a formatted notification is sent to the subscribed room.

### Deduplication

Duplicate delivery is prevented at two levels:

1. **In-memory LRU cache** (500 entries): Uses the GitHub `delivery_id` header
   for fast-path dedup. Entries are evicted when the cache is full.
2. **Persistent SQLite dedup table**: Uses a composed dedup key with a cooldown
   window (default 60 seconds). CI events for the same PR, check name, and
   conclusion are coalesced; non-CI events use the delivery ID.

### Quiet Hours

Notifications are suppressed during configurable quiet hours:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `quiet_start` | `23` | Hour to begin quiet period (0-23) |
| `quiet_end` | `8` | Hour to end quiet period (0-23) |

Quiet hours wrap around midnight: with defaults, notifications are suppressed
from 23:00 to 08:00. Set `quiet_start == quiet_end` to disable quiet hours.

### Rate Limiting

Per-room hourly rate limits prevent notification storms:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max_per_hour` | `0` (unlimited) | Max notifications per room per hour |

### Policy Decision Flow

Each notification goes through three sequential gates:

```
1. Dedup check (cooldown window)
   |-> Denied: "duplicate"
   v
2. Quiet hours check
   |-> Denied: "quiet_hours"
   v
3. Rate limit check
   |-> Denied: "rate_limited"
   v
Allowed -> deliver notification
```

Denied events are recorded in the activity ledger with the deny reason for
audit.

### CI Edit-in-Place

For CI events (check runs, check suites, workflow runs), the dispatch module
tracks message IDs by a composite key. When a CI event reaches a terminal state
(success, failure, cancelled, timed_out), it attempts to edit the existing
notification message rather than sending a new one. This keeps CI progress
updates concise in the room.

---

## 6. CI/Review Reporting

### Normalized Event Types

GitHub webhook events are normalized into typed variants:

| Webhook Event | Parsed Type | CI Summary | Review Summary |
|--------------|-------------|------------|----------------|
| `pull_request` | `PullRequest` | No | No |
| `issue_comment` | `IssueComment` | No | No |
| `pull_request_review` | `PullRequestReview` | No | Yes |
| `pull_request_review_comment` | `PrReviewComment` | No | Yes |
| `check_run` | `CheckRun` | Yes | No |
| `check_suite` | `CheckSuite` | Yes | No |
| `workflow_run` | `WorkflowRun` | Yes | No |

### CI Summary Fields

CI events are summarized into a normalized `ci_summary` with:

- `kind`: `CheckRun` | `CheckSuite` | `WorkflowRun`
- `name`: Check/workflow name
- `status` / `conclusion`: Current state
- `owner` / `repo`: Repository identifiers
- `pr_number`: Associated PR (if any)
- `head_sha`: Commit SHA
- `actor`: Who triggered the run
- `details_url`: Link to failing job details

### Review Summary Fields

Review events are summarized into a `review_summary` with:

- `state`: Normalized to `Approved` | `ChangesRequested` | `Commented` |
  `Dismissed` | `Pending` | `Unknown_review_state`
- `reviewer`: Review author
- `body`: Review body (truncated in notifications)
- `head_sha`: Commit SHA at review time

### Teams/Slack Rendering

Notifications are rendered with platform-appropriate link syntax:

**Markdown (Teams, Discord, Mattermost, etc.):**
```
PR #42 opened by @alice
Repository: owner/repo
Branch: feature/foo -> main
Title: Add new feature
URL: https://github.com/owner/repo/pull/42

---
[PR #42](https://github.com/owner/repo/pull/42) | [Check: CI](https://...)
```

**Slack mrkdwn:**
```
:large_green_circle: *PR #42* opened by @alice
Repository: owner/repo
Branch: `feature/foo` -> `main`
Title: Add new feature
URL: https://github.com/owner/repo/pull/42

---
<https://github.com/owner/repo/pull/42|PR #42> | <https://...|Check: CI>
```

### Mergeability Change Detection

PR events are also scanned for mergeability-relevant changes:

- `MergeableStateChanged` -- mergeable status toggled
- `LabelsChanged` -- labels added or removed (including `labeled`/`unlabeled`
  actions)
- `ReviewDecisionChanged` -- review decision updated
- `ChecksStatusChanged` -- aggregate check counts changed

---

## 7. Review Runs

Review runs provide automated code review and security scanning triggered by
labels, room commands, or manual CLI invocations.

### Run Kinds

| Kind | Description | Trigger Labels |
|------|-------------|---------------|
| `code_review` | Standard code review analysis | `review`, `code-review`, `needs-review` |
| `security_scan` | Security vulnerability scan | `security`, `security-review`, `security-scan` |
| `custom:<name>` | Custom review kind | N/A (room command or manual only) |

### Trigger Sources

| Source | Description |
|--------|-------------|
| `Label <name>` | Triggered by a GitHub label on the PR |
| `Subscription_rule` | Triggered by a subscription rule match |
| `Room_command` | Triggered by a room slash command |
| `Manual` | Manually triggered via CLI or API |

### Label-Triggered Runs

When a PR receives a label that maps to a review run kind, a review run is
automatically created:

1. Webhook receives a `pull_request` event with `labeled` action.
2. The label is checked against the label-to-run-kind mapping.
3. If matched, a review run is created (idempotent by `repo/PR/head_sha/run_kind`).
4. The run is queued as `pending` and picked up by a background worker.

Label matching is case-insensitive (`REVIEW` = `review` = `Review`).

### Idempotency

Review runs are idempotent by the composite key
`(repo, pr_number, head_sha, run_kind)`. Pushing the same commit again or
re-adding the same label will not create duplicate runs.

### Run Lifecycle

```
Pending  ->  Running  ->  Completed
                  \--->  Failed
```

Each state transition is recorded with timestamps. The `Running` state includes
an associated `task_id` for the background worker.

### Review Prompt Assembly

When a review run executes, the background task receives an enriched prompt
including:

- PR metadata (repo, number, title, author, branches, SHA)
- PR description (truncated to 2000 chars)
- Changed file list (up to 30 files, with additions/deletions counts)
- Trigger source and run kind
- Task-specific instructions (code review focus areas or security scan criteria)

### Workflow Runs

Workflow runs extend the trigger model for structured pipeline execution. They
are triggered from room commands, subscription rules, or manual CLI, and map to
a named structured pipeline with versioned inputs.

```bash
# Trigger a workflow run from a room (via slash command)
/workflow run deploy-pipeline --env=staging --version=1.2.3
```

Workflow runs follow the same lifecycle: `Pending -> Running -> Completed/Failed`.
Results are synced from the background task to the workflow run record.

---

## 8. Backlinks

The backlinks system maintains bidirectional cross-references between GitHub
items and Clawq room items for audit trails, retries, and provenance tracking.

### GitHub Item Types

| Type | Description |
|------|-------------|
| `pr_comment` | PR or issue comment |
| `pr_review_comment` | Review comment on a specific file/line |
| `pr_review` | PR review (approve/request changes/comment) |
| `branch` | Git branch |
| `commit` | Git commit |
| `workflow_run` | GitHub Actions workflow run |
| `check_run` | Individual check run |
| `check_suite` | Check suite |

### Room Item Types

| Type | Description |
|------|-------------|
| `message` | Room message |
| `thread` | Message thread |
| `background_task` | Background task execution |
| `review_run` | Review run record |
| `workflow_run` | Workflow run record |
| `artifact` | Generated artifact |

### Relationship Types

| Relationship | Direction | Description |
|-------------|-----------|-------------|
| `subscription_delivery` | GitHub -> Room | PR event delivered to subscribed room |
| `ci_notification` | GitHub -> Room | CI status delivered to room |
| `triggered_run` | Room -> GitHub | Room command triggered a review/workflow run |
| `provenance_comment` | Room -> GitHub | Background task posted result back to PR |

### Idempotency

Backlinks are idempotent via a UNIQUE constraint on:
```
(repo, github_item_type, github_item_id, room_id, room_item_type, room_item_id)
```

Optional ID fields use empty string defaults (not NULL) so the UNIQUE constraint
works correctly in SQLite (NULL != NULL would break dedup).

### Querying Backlinks

```ocaml
(* Find all backlinks from a specific GitHub item *)
Room_github_backlinks.find_by_github ~db ~repo:"owner/repo"
  ~github_item_type:Check_run ~github_item_id:"12345" ()

(* Find all backlinks from a specific room *)
Room_github_backlinks.find_by_room ~db ~room_id:"my-room"
  ~room_item_type:Message ()

(* Find all backlinks for a PR *)
Room_github_backlinks.find_by_repo_pr ~db ~repo:"owner/repo"
  ~pr_number:42 ()

(* Count backlinks for a room *)
Room_github_backlinks.count_by_room ~db ~room_id:"my-room" ()
```

### Cleanup

Old backlinks can be purged by timestamp:

```ocaml
Room_github_backlinks.delete_before ~db
  ~before_timestamp:"2024-01-01T00:00:00" ()
```

---

## 9. Troubleshooting

### Webhook Events Not Arriving

1. **Check webhook secret**: Ensure the secret in the GitHub App settings
   matches the config. Per-repo secrets override the app-level secret.
2. **Verify installation ID**: The `installation_id` in the webhook payload
   must match a configured installation.
3. **Check repo scope**: The repo must be in the installation's repo list (or
   the list must be empty for all repos).
4. **Check allow_users**: If `allow_users` is set, the triggering user must be
   in the list.

### Notifications Suppressed

1. **Check subscription enabled**: `clawq subscriptions show <ID>` -- verify
   `enabled: yes`.
2. **Check notification preferences**: Each event type has its own toggle.
3. **Quiet hours**: Default is 23:00-08:00. Check current server time.
4. **Dedup cooldown**: Default 60 seconds. CI events are coalesced by
   `(repo, pr, check_name, conclusion)`.
5. **Rate limiting**: Check if `max_per_hour` is set and the room has hit the
   limit.

### Review Runs Not Triggering

1. **Label mapping**: Only specific labels trigger runs (`review`, `code-review`,
   `needs-review`, `security`, `security-review`, `security-scan`). Matching is
   case-insensitive.
2. **Idempotency**: A run with the same `(repo, PR, head_sha, run_kind)` will
   not be created twice. Check existing runs.
3. **Subscription scope**: Subscription rule triggers require an active
   subscription.

### Token Issues (GitHub App)

1. **Private key path**: Verify the PEM key exists and is readable.
2. **Key format**: Must be RSA (PKCS#8 or PKCS#1). Other key types are
   rejected.
3. **App ID**: Must match the GitHub App's ID.
4. **Token cache**: Tokens are cached for 50 minutes. Use
   `invalidate_all()` to force refresh.
5. **Egress rules**: If egress policy rules are configured, verify that
   `api.github.com` is allowed.

### Rate Limit / 403 Errors

- GitHub App installation tokens are subject to GitHub's rate limits
  (5000 requests/hour for authenticated apps).
- PAT tokens share the user's rate limit (also 5000/hour).
- If using a GitHub Enterprise Server, set `CLAWQ_GITHUB_API_BASE` to the
  correct API endpoint.

### Schema Migrations

The PR subscription schema is automatically migrated on database init. If you
see schema errors, ensure the database file is writable and the schema version
is consistent. The migration adds missing columns (e.g. `enabled`) via
`ALTER TABLE` with fallback for already-existing columns.
