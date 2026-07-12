# P19-P22 Integrated Plan Review Receipt

Date: 2026-07-13
Result: PASS

## Scope

This review re-opened the complete P19-P22 planning surface after the original
P19/P20 ingest review. It covered canonical plans, owning backlog task bodies,
dependency edges, estimates, phase-boundary rollout semantics, the Principal
and token-vault security model, Connector ingress trust, and the P22
documentation/feature-catalog maintenance contract.

Current inventory:

- P19: 53 tasks, 231 hours.
- P20: 13 tasks, 53 hours.
- P21: 70 tasks, 328 hours.
- P22: 18 tasks, 94 hours.
- Combined: 154 tasks, 706 hours.

## Initial independent review

Independent P21, P22, and cross-phase reviewers returned FAIL. Their material
findings were:

1. Principal merge, survivor/adoption, unlink, split, and revocation semantics
   were absent.
2. Connector identity tasks did not own cryptographic/transport trust and Teams
   identity could be derived from unverified claims.
3. The vault implied whole-store rollback detection that record AEAD and CAS
   alone cannot provide.
4. Master-key lifecycle, rewrap, backup/restore, and compromise recovery were
   missing.
5. Security verification did not depend on the full owning implementation
   surface.
6. Delayed work and webhook reconciliation did not preserve an unambiguous
   actor/authorization lineage.
7. P19 high-risk App attribution and the P21 `User_required` transition had no
   explicit fail-closed rollout boundary.
8. Device authorization was coupled to web authorization internals instead of
   a shared verified activation seam.
9. P22 feature visibility had no verified caller-audience input.
10. The Claude-tag parity audit lacked a reproducible, repository-owned source
    manifest and evidence crosswalk.
11. GitHub documentation coverage was not a canonical machine-readable matrix.
12. Feature catalog versioning and lifecycle rules were incomplete.
13. The final documentation gate did not own parity, CI, Pages, navigation, and
    orphan-page checks.
14. Several tasks combined too many independently risky behaviors and their
    estimates and parent rollups were no longer credible.

## Repairs

The owning plan and backlog contracts now:

- define collision-safe Principal identity, two-sided private linking,
  deterministic adoption, unlink/split, credential ownership, and revocation;
- validate Teams JWTs against Microsoft OpenID/JWKS and bind service URL,
  audience, issuer, time, and canonical actor claims;
- bind Slack identities to the app-token-authenticated Socket Mode connection,
  app/workspace namespace, envelope identity, acknowledgements, and replay
  handling;
- bind Discord identities to the bot-token-authenticated Gateway session,
  Ready application identity, intents, guild/user IDs, and monotonic sequence;
- bind Telegram identities to bot-token-authenticated HTTPS long polling,
  bot namespace, immutable `from.id`, monotonic update offsets, and restart
  deduplication;
- separate shared credential activation from the web and device authorization
  transports;
- add versioned master-key loading, staged rewrap, backup/restore, compromise
  recovery, and explicit limits on rollback detection without an external
  monotonic anchor;
- preserve immutable actor evidence through confirmations, delayed jobs,
  retries, receipts, and webhook reconciliation while re-resolving current
  authority immediately before execution;
- keep every high-risk P19 App-attributed action off by default outside one
  named, time-bounded pilot, then require an audited P21 `User_required`
  transition with no App/PAT fallback when user authorization is unavailable;
- split reviewer requests from PR review submission so only the former follows
  ordinary metadata policy and the latter follows the high-risk pilot/rollout
  gate;
- add a canonical GitHub coverage matrix, hashed parity-source manifest,
  machine-readable evidence crosswalk, versioned feature lifecycle schema,
  verified caller audience, and repository-enforced docs generation/CI/Pages
  contract; and
- split the broad work into explicit tasks and repair all phase, milestone,
  epic, and duplicated parent-summary estimates.

The domain decisions are recorded in ADRs 0005-0007 and the project glossary in
`CONTEXT.md`.

## Protocol references

The ingress contracts were checked against the current official protocol
documentation:

- [Microsoft Bot Connector authentication](https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-authentication?view=azure-bot-service-4.0)
- [Slack Socket Mode](https://docs.slack.dev/apis/events-api/using-socket-mode/)
- [Discord Gateway](https://docs.discord.com/developers/events/gateway)
- [Telegram Bot API](https://core.telegram.org/bots/api)
- [GitHub user access tokens](https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/generating-a-user-access-token-for-a-github-app)

## Verification evidence

- Canonical task inventory: all 154 plan rows mechanically match backlog task
  IDs, titles, estimates, and dependency ordering.
- Parent estimate audit: every epic equals its leaf-task sum, every milestone
  equals its epic sum, every phase equals its milestone sum, and duplicated
  parent summaries match their child indexes.
- Expected totals: P19 231h, P20 53h, P21 328h, P22 94h; combined 706h.
- `bl check --strict`: passed with no issues.
- `git diff --check c44d6608`: passed for the complete repaired surface.
- Final independent rereview: PASS; no remaining material gap or regression.

All P19-P22 implementation tasks remain pending. This work repaired planning,
security, rollout, documentation, and verification contracts; it did not claim
feature implementation.
