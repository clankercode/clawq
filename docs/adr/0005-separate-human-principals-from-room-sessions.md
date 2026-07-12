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

Only Connector actors whose immutable, scoped user ID was established by that
Connector's trusted ingress verification can resolve to a human Principal.
Unverified Teams claims, web request fields, CLI arguments, direct-call metadata,
display names, email strings, and matching external accounts are evidence at
most: they never create or resolve a Connector actor and never authorize work.
Verified first-seen Connector actors receive a Principal.

Cross-Connector linking requires an expiring private proof completed by both
actors, or an audited admin plan-confirm-apply repair. If both actors already
own Principals, linking is a Principal merge. The ordinary two-sided flow picks
the Principal with the earlier durable creation order as the survivor, breaking
an exact tie by stable Principal ID; an admin repair plan may instead name the
survivor explicitly. The plan previews the survivor, the merged Principal, and
every state conflict before confirmation.

Apply is one atomic, compare-and-swap operation. It adopts every Connector actor
and all non-conflicting Principal-owned state into the survivor, then replaces
the other Principal with an immutable `merged_into` tombstone. Reads of the former
ID follow that alias to the current survivor; the tombstone cannot own actors,
bindings, credentials, preferences, transactions, or work. Replaying the same
proof or repair revision returns the already-completed result. Concurrent merge
or unlink attempts serialize on the current Principal roots, reject stale
plans, and cannot create alias cycles or leave partially adopted state.

Merge conflict policy is deliberately fail closed:

- Identical external-account bindings coalesce without copying credential
  authority. Distinct bindings that violate an account or App uniqueness rule
  block apply until a new audited plan explicitly keeps one and revokes the
  other; credentials are never silently overwritten.
- Non-conflicting preferences are adopted. Conflicting preference values use
  the selected survivor's value and are enumerated in the preview and receipt.
- Pending authorization transactions are invalidated rather than rebound.
- Delayed jobs and historical records retain their immutable Actor snapshot and
  original confirmation and authorization lineage. Their Actor may resolve
  through the tombstone for current attribution, but the merge never lets old
  evidence acquire a survivor credential or permission it did not already
  authorize; protected execution must revalidate or request new confirmation.

Unlinking a Connector actor is an identity split, not a reverse credential
merge. It atomically moves that actor to a new, empty Principal with default
preferences; no binding, credential, pending authorization transaction, or
authority is transferred automatically. The former Principal retains its state
even if it now has no actors, in which case it cannot authorize actor-originated
work and requires an audited repair to become reachable again. Historical
snapshots subsequently resolve through the actor's live link to the new
Principal and therefore fail protected work until that Principal is
independently bound and authorized.

## Consequences

- Two people in one Room cannot resolve one another's account or credential.
- Equal Connector user IDs in different tenants remain different Actors.
- Connector and GitHub display-name changes do not change identity.
- Legacy rows without unambiguous immutable provenance remain readable but
  cannot authorize user-attributed work.
- A merge preserves stable references without treating the losing Principal as
  a second credential owner.
- Linking and unlinking can invalidate pending or delayed work rather than
  silently broadening its authority.
- Principal disable, Actor unlink, and Connector offboarding can invalidate all
  derived authorization immediately.
