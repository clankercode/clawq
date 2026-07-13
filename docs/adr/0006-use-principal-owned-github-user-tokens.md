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
confirmation. A web PKCE verifier is held only by an opaque secret handle and,
on every terminal callback outcome, is deleted together with its protected
metadata. Require expiring user tokens. Store access/refresh material in a
fail-closed mutable encrypted vault with generation-based CAS rotation. A token
generation versions one binding's mutable GitHub access/refresh lineage; it is
not an encryption-key version. Refresh, revoke, and lease CAS compare the token
generation even when the record remains encrypted by the same vault master key.

Vault master keys come from an external key source, not from the credential
database. Each encrypted record carries a versioned key ID and authenticated
context, and the external keyring declares the active key ID for new writes.
Master-key rotation is staged and crash-safe: install the new key, make it the
active write key, rewrap records individually under CAS while retaining the old
key, verify complete coverage across restart and the supported backup set, then
retire the old key only after no live record or retained backup requires it.
Because every record identifies its key, an interrupted rotation resumes with
both versions and never guesses which key to use.

A backup is restorable only with its database snapshot, its declared required
key IDs, and an operator-approved keyring that contains those versions. Missing
key material fails closed. Restore starts with user authorization disabled,
discards restored access-token leases, and forces each binding through GitHub
refresh and identity validation before rewriting it under the active key;
bindings that cannot reconcile remain disabled. Record AEAD and token-generation
CAS do not detect replacement of the entire database with an internally
consistent older snapshot encrypted under an available key: that requires a
monotonic anchor outside both the database and its backup. V1 therefore makes
no whole-store anti-rollback claim; backup selection and restore authorization
are an explicit operational trust boundary.

Suspected master-key compromise or unrecoverable loss has no in-place recovery
shortcut. Clawq disables all affected Principal-owned GitHub authorization,
revokes upstream credentials where possible, destroys the affected local
credential references and bindings, and requires users to relink. It never
falls back to App attribution for a `User_required` action during recovery.

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
- Refresh-token rotation and vault-key rotation have independent version
  histories and recovery procedures.
- Whole-store rollback resistance remains out of scope until Clawq has an
  external monotonic anchor.
- Key rotation can resume after a crash, while key compromise or unrecoverable
  loss requires destructive disable-and-relink recovery.
- Authenticated Git transport and commit authorship require a separate design.
