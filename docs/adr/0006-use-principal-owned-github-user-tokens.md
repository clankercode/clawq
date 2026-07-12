# 6. Use Principal-owned GitHub App user tokens for attributed actions

Date: 2026-07-13
Status: Accepted

## Context

GitHub App installation tokens attribute actions to the App. Some explicit Room
actions should instead carry GitHub's native user-plus-App attribution, without
introducing shared PATs, exposing tokens to coding runners, or turning OAuth
authorization into blanket consent for later mutations.

## Decision

Use GitHub App user access tokens bound to a Principal and verified GitHub
numeric user ID. Support state plus S256 PKCE web authorization and GitHub App
device authorization through private, restart-safe transactions. Successful
authorization verifies the GitHub identity and seals pending credentials, then
resumes a redacted, revision-bound binding plan; it never counts as apply
confirmation. Require
expiring user tokens and store access/refresh material in a fail-closed mutable
encrypted vault with generation-based CAS rotation.

Every GitHub mutation declares `App`, `User_required`, or `User_preferred`.
OAuth permits an explicitly confirmed account binding; action confirmation
remains separate. Raw user
tokens are leased only inside the GitHub HTTP request boundary and never enter
Rooms, prompts, tools, jobs, runners, shell, Git transport, receipts, or audit.
Delayed work pins identity and confirmation references and revalidates current
authority without switching actors.

## Consequences

- GitHub, not Clawq-authored text, supplies native user-plus-App attribution.
- Low-risk App fallback is possible only when policy permits and the preview
  named it; `User_required` never falls back.
- Ambient and scheduled automation remains App-attributed.
- Refresh races, tampering, revocation, SSO/permission loss, and missing keys
  fail closed and require repair or relinking.
- Authenticated Git transport and commit authorship require a separate design.
