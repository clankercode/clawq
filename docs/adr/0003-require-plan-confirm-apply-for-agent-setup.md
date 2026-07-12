# 3. Require plan-confirm-apply for agent-assisted setup

Date: 2026-07-12
Status: Accepted

## Context

Agent-assisted setup changes durable routes, Room access, credentials, and
external GitHub App state. Natural-language intent alone is insufficient consent,
especially when targeting another Room or resuming after a browser callback.

## Decision

All setup adapters use one typed plan-confirm-apply framework. Planning is
read-only and produces a redacted, expiring, revision-bound structured diff.
Apply requires the matching plan ID and digest, rechecks authority and readiness,
and is atomic, idempotent, and audited.

A global admin may target another Room. Otherwise the destination Room's admin
must consent. Setup-owned access bundles are provenance-tracked and detach only
after their last managed feature is removed; manual grants remain untouched.

## Consequences

- Browser callbacks resume setup but never count as apply confirmation.
- Concurrent or stale plans fail before mutation.
- GitHub is the first adapter; the existing room-agent pilot is ported later.
- Secret-bearing Connector onboarding remains separate adapter work.

