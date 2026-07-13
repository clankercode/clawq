# P21.M3.E1 Refresh and Revocation Lifecycle Review Receipt

Date: 2026-07-14

Result: BLOCKED — production refresh and App-authorization revocation are not
wired to a secure daemon runtime boundary.

## Scope

- P21.M3.E1.T001–T004
- `github_user_token_refresh`
- `github_user_auth_revocation_webhook`
- account-admin invalidation and Connector split lifecycle

## Verified repair

Connector split now propagates canonical invalidation failure through the
enclosing SQLite transaction.  A forced invalidation-receipt insert failure
refuses the split and leaves actor ownership, the active identity link, and the
pending-authorization count unchanged.

## Remaining block

The refresh and webhook library implementations are present and their focused
tests pass, but neither is reachable from the production daemon with the
required secure inputs:

- `Github_user_token_refresh.acquire_lease` requires a vault key provider, an
  HTTP boundary, a client-secret resolver, and a client-id handle.
- `Github_user_auth_revocation_webhook.process` requires the same vault key
  provider, plus the configured App identity and signed delivery request.
- The daemon's GitHub configuration provides only App ID, private-key path,
  webhook secret, and installation scope. It has no GitHub App user-OAuth
  client credential or vault-key-provider runtime boundary.
- The reviewed local pilot configuration is PAT-only and has no configured
  user-refresh or vault-key inputs. No credential values are recorded here.

Adding a plaintext configuration fallback or constructing an in-process key
provider from an unchecked secret would violate the vault's fail-closed
contract. The required follow-up is a separately scoped runtime credential and
key-provider lifecycle, then production dispatch/webhook integration tests and
the associated operator documentation updates.

## Verification evidence

- `make test-run ARGS='test principal_unlink_split'`
- `make test-run ARGS='test github_user_token_refresh'`
- `make test-run ARGS='test github_user_auth_revocation_webhook'`
- `make test-run ARGS='test github_user_auth_invalidate'`
- `make test-run ARGS='test github_account_admin_surface'`
- `make fmt-check`
- `git diff --check`
