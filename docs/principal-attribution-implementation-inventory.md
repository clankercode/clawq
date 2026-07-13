# P21 implementation inventory (task / source / schema / API / test)

Crosswalk of shipped P21 surfaces for operators and reviewers (P21.M4.E3.T002).
Not exhaustive of every helper; names major modules and regression suites.

## Identity and linking

| Concern | Source | Schema / store | Tests |
|---------|--------|----------------|-------|
| Principal / actor model | `principal_identity` | types | `principal_identity` |
| Persist Principals/links | `principal_identity_store` | SQLite principals/actors/links | `principal_identity_store` |
| Bootstrap fail-closed | `principal_bootstrap` | — | `principal_bootstrap` |
| Resolve/create | `principal_resolve` | store | `principal_resolve` |
| Ingress adapters | `teams/slack/discord/telegram_principal_*` | — | `*_principal` suites |
| Link protocol | `principal_link_protocol` | types | `principal_link_protocol` |
| Link execution | `principal_link_exec` | `principal_link_tx`, edges | `principal_link_exec` |
| Merge/adopt | `principal_merge`, `principal_merge_persist` | snapshots/receipts | `principal_merge` |
| Unlink/split | `principal_unlink_split` | plans | `principal_unlink_split` |
| Legacy migrate | `principal_legacy_migrate` | migration tables | `principal_legacy_migrate` |
| Isolation proof | — | — | `principal_isolation_attribution` |

## Account binding and preferences

| Concern | Source | Tests |
|---------|--------|-------|
| Bindings + vault ref | `github_account_binding` | `github_account_binding` |
| Ownership policy | `github_account_ownership_policy` | `github_account_ownership_policy` |
| Preferences | `github_account_preference` | `github_account_preference` |
| Eligible resolve | `github_eligible_account_resolve` | `github_eligible_account_resolve` |
| Admin surface | `github_account_admin_surface` | `github_account_admin_surface` |
| CLI / tool | `github_account_cli`, `github_account_tool` | `github_account_cli` |

## Authorization and vault

| Concern | Source | Tests |
|---------|--------|-------|
| Readiness | `github_user_auth_readiness` | `github_user_auth_readiness` |
| Auth tx | `github_user_auth_tx` | `github_user_auth_tx` |
| Private delivery | `github_user_auth_delivery` | `github_user_auth_delivery` |
| PKCE | `github_user_auth_pkce`, `*_callback` | `github_user_auth_pkce*` |
| Device | `github_user_auth_device`, `*_poll` | `github_user_auth_device*` |
| Activate | `github_user_auth_activate` | `github_user_auth_activate` |
| Token records | `github_user_token_store` | `github_user_token` |
| Master key | `github_user_token_master_key` | `github_user_token_master_key` |
| Vault CRUD | `github_user_token_vault` | `github_user_token_vault` |
| CAS | `github_user_token_cas` | `github_user_token_cas` |
| Lease | `github_user_token_lease` | `github_user_token_lease` |
| Refresh / flight | `github_user_token_refresh` | `github_user_token_refresh` |
| Rewrap | `github_user_token_rewrap` | `github_user_token_rewrap` |
| Recovery | `github_user_token_vault_recovery` | `github_user_token_vault_recovery` |
| Invalidate | `github_user_auth_invalidate` | `github_user_auth_invalidate` |
| Revocation webhook | `github_user_auth_revocation_webhook` | `github_user_auth_revocation_webhook` |
| Enablement admin | `github_user_auth_enablement` | `github_user_auth_enablement` |
| Diagnostics | `github_user_auth_diagnostics` | `github_user_auth_diagnostics` |
| App/PAT compat | `github_app_pat_compat` | `github_app_pat_compat` |
| Security suite | — | `github_p21_security`, vault_security |

## Attribution and actions

| Concern | Source | Tests |
|---------|--------|-------|
| Policy | `github_attribution_policy` | `github_attribution_policy` |
| Authorize | `github_attribution_authorize` | `github_attribution_authorize` |
| Fallback | `github_attribution_fallback` | `github_attribution_fallback` |
| Dispatch lease | `github_attribution_dispatch_lease` | `github_attribution_dispatch_lease` |
| Audit | `github_attribution_audit` | `github_attribution_audit` |
| Rollout | `github_attribution_rollout` | `github_attribution_rollout` |
| Actor pin | `actor_snapshot`, `github_action_actor_attribution` | `actor_snapshot`, `github_action_actor_attribution` |
| Delayed pin | `github_delayed_attribution` | `github_delayed_attribution` |
| Family integrators | `github_*_attribution` (collab, PR review, issue, merge, workflow_dispatch, code_change) | matching suites |
| Reconcile | `github_action_reconcile` | `github_action_reconcile` |
| Integration | — | `github_p21_integration` |

## Docs and pilots

| Artifact | Path |
|----------|------|
| Plan | `docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md` |
| ADR boundary | `docs/adr/0009-principal-token-vault-security-boundary.md` |
| Glossary | `docs/glossary-principal-github-attribution.md` |
| Operator contract | `docs/github-user-auth-operator-contract.md` |
| Vault recovery | `docs/github-vault-recovery.md` |
| Pilot runbook/checklist/receipt/backout | `docs/pilots/p21-*` |
| Doc drift tests | `github_p21_docs`, `github_p21_adr_glossary`, `github_p21_pilot_dryrun` |

## API stability notes

- Prefer pure modules with injectable fetchers/clocks for tests.
- Redacted JSON/export paths must never include access/refresh tokens, client
  secrets, device codes, or raw vault plaintext.
- Minimal build: disabled guidance for integration-only CLI surfaces.
