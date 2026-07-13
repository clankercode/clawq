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
