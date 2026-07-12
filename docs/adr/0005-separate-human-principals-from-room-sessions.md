# 5. Separate human Principals from Room Sessions

Date: 2026-07-13
Status: Accepted

## Context

Clawq currently carries Connector sender identifiers and display metadata into
Room Sessions and delayed work. A shared Room Session contains several humans,
and a Session may outlive a Connector display name or the message that started a
job. Using the Session or a requester string as credential ownership would let
one participant inherit another participant's GitHub authorization.

## Decision

Introduce a durable Principal as Clawq's human identity. A Connector actor is a
verified tuple of Connector, tenant/workspace/account scope, and immutable
Connector user ID linked to one Principal. A Room and its Session are execution
contexts, never identity or credential owners.

Verified first-seen Connector actors receive a Principal. Cross-Connector linking requires
an expiring private proof completed by both actors, or an audited admin
plan-confirm-apply repair. Display names, email strings, and matching external
accounts never auto-link Principals. Turns and delayed work retain immutable
actor snapshots as evidence but re-resolve current Principal authority before
each protected action.

## Consequences

- Two people in one Room cannot resolve one another's account or credential.
- Equal Connector user IDs in different tenants remain different Actors.
- Connector and GitHub display-name changes do not change identity.
- Legacy rows without unambiguous immutable provenance remain readable but
  cannot authorize user-attributed work.
- Principal disable, Actor unlink, and Connector offboarding can invalidate all
  derived authorization immediately.
