# 2. Use unified live GitHub App routes

Date: 2026-07-12
Status: Accepted

## Context

Clawq already has GitHub App authentication, repository grants, and per-PR Room
subscriptions. The requested behavior must cover PRs and Issues at Item, Repo,
and Org scope, including repositories added to an installation later. Maintaining
parallel per-repository webhook and subscription models would duplicate matching,
delivery, setup, and authorization behavior.

## Decision

Use one verified GitHub App webhook ingress and one versioned route model with
Item, Repo, and Org selectors. Org scope is the live App installation scope.
Resolve routes per destination using Item > Repo > Org specificity, selecting the
most-specific configured route before enabled/filter evaluation. Migrate existing
per-PR subscriptions into Item routes and preserve their CLI as compatibility
aliases.

PAT authentication remains exact-Repo compatibility only. It cannot provide Org
installation semantics.

## Consequences

- Newly granted repositories can join an Org route without config edits.
- Narrow disabled/filtered routes can intentionally mute broader feeds.
- All event consumers share normalized PR/Issue envelopes.
- Migration and compatibility tests are required before the old subscription
  storage stops being authoritative.

