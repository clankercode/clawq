# 8. GitHub route model, App setup, and operator contract

Date: 2026-07-13
Status: Accepted

## Context

P19 unifies live GitHub App ingress with Item/Repo/Org routes into Rooms (and
direct Sessions). ADR 0002 chose one webhook path and specificity-ordered
selectors; ADR 0003 chose typed plan-confirm-apply for agent-assisted admin.
Operators and agents still need a single written contract for precedence,
App vs PAT limits, callback resume, delivery independence, readiness/repair, and
secret redaction so docs, CLI/agent surfaces, and tests cannot drift.

Canonical product plan:
[docs/plans/2026-07-12-github-item-room-routing.md](../plans/2026-07-12-github-item-room-routing.md).
Operator procedures:
[docs/github-route-operator-contract.md](../github-route-operator-contract.md).
Terms: [docs/glossary-github-routes.md](../glossary-github-routes.md).

## Decision

### Route model (extends ADR 0002)

1. **Selectors.** Routes use `Item` (repo + PR|Issue + number), `Repo`
   (`owner/repo`), or `Org` (installation/org name). Destination is Room-first
   or a direct Session key.
2. **Specificity and no-fallthrough.** Resolution is destination-local. Among
   selectors that apply to a normalized envelope, the most-specific *configured*
   class wins (`Item > Repo > Org`) *before* enabled/filter evaluation. A
   disabled or filter-rejected narrow route yields **Muted** and never falls
   through to a broader route. That is intentional fail-closed mute.
3. **Uniqueness.** At most one *active* (enabled) route per
   `(destination, canonical selector)`. Soft disable/remove frees the slot;
   hard delete is not required for lifecycle.
4. **Acceptance bound.** After match, at most one accepted routed event per
   destination for a given GitHub delivery plus canonical item (durable ledger).
5. **Auth scope.** Org routes require live GitHub App installation scope
   (`can_claim_org_scope`). PAT authentication is exact-Repo compatibility only
   and cannot claim Org. Suspended/deleted installations and selected-repo
   removals fail closed.

### Setup (extends ADR 0003)

1. **Plan-confirm-apply only.** Route create/update/disable/remove and App
   onboarding produce redacted, expiring, revision-bound `Setup_plan` values.
   Planning stores pending plans; it never mutates routes or installation state.
2. **Callback is not apply.** Browser/manifest callback exchange verifies and
   consumes one-time state, then **resumes** into the originating Room (or a
   notification) with readiness and a confirmable plan. The
   callback never counts as confirmation; plan-confirm-apply is still required.
3. **Apply gates.** Apply requires plan id + digest, principal, base revision,
   destination authority (global admin or destination Room admin), and for Org
   selectors an Active App installation that can claim org scope. The full CLI
   takes the independently supplied `CLAWQ_PRINCIPAL_ID`; it never copies the
   stored plan principal into the apply request. Apply is atomic, idempotent,
   and durably audited. Direct Session App setup requires global-admin
   authority and an explicit matching Session destination.
4. **Managed access.** Confirmed Room-bound App setup attaches a
   setup-owned Room access bundle/feature in the same apply transaction.
   Removing/disabling the last managed route detaches only setup-owned linkage;
   manual grants stay. The same transaction records a durable next-turn catalog
   refresh request; the next Room turn consumes it before freezing its Tool
   catalog, without daemon restart.
5. **Secrets.** Credential material (PEM, client secret, webhook secret, tokens)
   is stored only through the credential boundary. Plans, inspect views, audit
   details, and Channel messages carry handles/public metadata only; redaction
   is mandatory on export and audit paths.

### Webhook ingress and delivery independence

1. **One verified App ingress.** Shared path (default `/github/app/webhook`)
   checks signature, delivery id, installation/repo authorization, and allowed
   event class before normalization or routing work.
2. **Delivery ACK independent of Connector.** HTTP acknowledgement and durable
   delivery-id ledger recording are independent of Connector fan-out, card
   render, and outbox success. Retries after ACK become duplicates at the
   ledger; Connector/outbox failures do not require GitHub redelivery for
   idempotency correctness.
3. **Webhook does not wake the agent.** Ingress and routing update journals and
   projections only. Agent turns require explicit Room reply, mention, or
   supported card action (ADR 0004).

### Operator surface

The full-build CLI exposes `github route plan|inspect|list|change|disable|remove|apply`,
`github app deliveries|apply`, and `github diagnostics route|audit`.
Planning/apply commands require `CLAWQ_ADMIN=1` and
`CLAWQ_PRINCIPAL_ID=…`; inspection/diagnostics do not. The minimal binary
reports the entire `github` route/App setup surface as disabled rather than
partially applying routes. Networked GitHub App webhook/setup features live in
`clawq_runtime_integrations`.

## Consequences

- Narrow muted routes are a supported mute tool, not a bug.
- PAT deployments stay Repo-scoped; operators must migrate to App for Org.
- Resume-after-callback and plan expiry force re-plan rather than silent apply.
- Documentation drift is checked by phrase/existence tests on the ADR, operator
  contract, and glossary.
- Delivery and Connector diagnostics are separate repair tracks from route match
  and App installation readiness.

## Related

- [ADR 0002 — unified live GitHub App routes](0002-use-unified-live-github-app-routes.md)
- [ADR 0003 — plan-confirm-apply for agent setup](0003-require-plan-confirm-apply-for-agent-setup.md)
- [ADR 0004 — Room Session owns GitHub event context](0004-room-session-owns-github-event-context.md)
