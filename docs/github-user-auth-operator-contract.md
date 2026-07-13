# GitHub user-auth operator contract

Safe defaults and actionable failure states for Principal-bound GitHub user
authorization and attributed actions (P21). No secrets or unsupported
guarantees. Normative boundary: [ADR 0009](adr/0009-principal-token-vault-security-boundary.md).

## Safe defaults

| Surface | Default |
|---------|---------|
| Production user-auth enablement | **Off** (`safe_default`) |
| High-risk action pilots | **Off** unless time-bounded and audited |
| `User_required` without token | **Deny** with repair (no silent App/PAT) |
| `User_preferred` App fallback | Only if policy permits **and** preview names App |
| Master key | External source; fail closed if missing/wrong/inaccessible |
| Vault restore | User-auth disabled until revalidation |
| Minimal build | User-auth / account CLI surfaces return disabled guidance |

## Operator surfaces

| Intent | Entry (full build) |
|--------|--------------------|
| Account list/status/use/link/unlink | `clawq github account …` |
| Admin enablement readiness/repair | `clawq github user-auth status|readiness|repair|enable|disable|apply` |
| Diagnostics (redacted) | module `Github_user_auth_diagnostics` (metrics/status export) |
| Attribution rollout | `Github_attribution_rollout` gates; pilot docs under `docs/pilots/` |

## Failure states (actionable, secret-free)

| Class | Operator action |
|-------|-----------------|
| Trust adapter / identity failure | Fix Connector auth; never link by display/email |
| Linking conflict / ambiguity | Private repair; admin plan-confirm-apply only |
| Authorization incomplete | Re-run private web/device continuation; confirm activation |
| Key missing / wrong / inaccessible | Restore external key; do not paste keys into config exports |
| Refresh CAS / single-flight | Wait or relink if fail-closed after remote rotation |
| Revocation (webhook or local) | Expect binding revoked, generation advanced, leases dead |
| Attribution deny (SSO/permission/scope) | Surface named class; reconfirm or fix Org/SSO |
| Lineage break after split/relink | Re-pin delayed jobs; old snapshots never re-authorize |
| Backup/restore | Operator proof required; destroy leases; revalidate bindings |
| Pilot cleanup | Rollback → cleanup → `no_residual_authority` → safe_default |

## Compatibility

When user-auth is disabled/unconfigured, App installation reads and
policy-permitted App actions retain deterministic App/PAT behavior. PAT remains
exact-Repo only (`Github_app_pat_compat`). Minimal runtime must not link
integration-only network paths for user-auth.

## Legacy requester migration (P21.M1.E3.T003)

Daemon database startup runs the idempotent `Principal_legacy_migrate` upgrade
before it makes the database available to dispatchers. The upgrade only
backfills a legacy requester only when its active, adapter-verified Connector
actor and active identity link resolve to a live Principal. It never treats an
unlinked actor's stored Principal, display names, emails, room IDs, or sessions
as authority. A failed upgrade prevents the daemon from using that database;
fix the database error and restart rather than bypassing the check.

Inspect the migration report in the daemon log first (run ID, `backfilled`,
`unresolved`, and `jobs_invalidated`). With the daemon stopped and a database
backup retained, an operator can inspect the durable report without exposing
tokens:

```sql
SELECT run_id, started_at, finished_at, backfilled, unresolved, jobs_invalidated,
       rolled_back
FROM principal_legacy_migration_runs
ORDER BY started_at DESC;

SELECT source_kind, source_id, status, unresolved_reason, principal_id,
       created_at
FROM principal_legacy_migration_records
WHERE run_id = :run_id
ORDER BY source_kind, source_id;

SELECT source_kind, source_id, reason, invalidated_at
FROM principal_legacy_invalidated_jobs
WHERE run_id = :run_id;
```

`legacy_unresolved` means the row remains available for read/audit and for an
explicit policy-permitted App/PAT operation, but it has no human authority.
Do not "repair" it by guessing from a display name. A human-attributed retry
must supply a verified Connector identity and create a new request; the worker
rejects an unresolved or `job_invalidated` legacy source rather than silently
authorizing it.

An invalidated active legacy job is not safe to resume as its old requester.
Keep its original evidence for audit, inspect the invalidation report, verify
the initiator through the Connector, then re-plan/re-enqueue a new durable job.
Cancellation preserves any immutable actor snapshot; retry/recovery re-resolve
it and fail closed if it is revoked, split, stale, or malformed.

Rollback is a controlled maintenance operation, not a way to restore user
authority. Stop human-attributed dispatch, back up the database, invoke
`Principal_legacy_migrate.rollback_run ~db ~run_id`, verify that the matching
records and invalidations are removed while historical snapshots remain, then
restart so the idempotent upgrade produces a fresh report. Do not delete
Principal, identity-link, or actor-snapshot rows manually.

## Explicit non-guarantees

- Live Teams dual-attribution pilot is not claimed without credentials/room/webhook.
- Whole-store vault rollback under the same key is not auto-detected without an
  external monotonic anchor.
- Authorization success is not GitHub mutation apply confirmation.

## Related

- [Implementation inventory](principal-attribution-implementation-inventory.md)
- [Glossary](glossary-principal-github-attribution.md)
- [Rollout/backout guide](pilots/p21-rollout-backout-guide.md)
- [Vault recovery](github-vault-recovery.md)
