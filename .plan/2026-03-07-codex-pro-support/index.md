# I006: Codex Pro Subscription Support - Plan

**Plan directory:** `.plan/2026-03-07-codex-pro-support/`
**Date:** 2026-03-07
**Status:** Planning complete, ready for implementation
**Backlog idea:** I006

---

## Summary

Add support for using a ChatGPT/Codex Pro subscription as an OpenAI Codex provider in clawq.

This is not the same as existing OpenAI-compatible API key support. It needs:
- PKCE OAuth login against `https://auth.openai.com`
- refreshable stored credentials instead of a static `api_key`
- runtime routing for Codex models through `https://chatgpt.com/backend-api/codex`
- optional `ChatGPT-Account-Id` header support for org-backed subscriptions
- CLI and wizard flows that work both locally and in remote/headless environments

---

## Research Findings

### clawq today

- `src/runtime_config.ml` only models provider auth as `api_key`.
- `src/provider.ml` treats OpenAI-family providers as static bearer-token API key integrations.
- `src/config_validate.ml` assumes OpenAI validation means `GET <base_url>/models`.
- `src/command_bridge.ml` has no upstream provider login/logout flow.

Conclusion: Codex Pro support does not already exist in clawq.

### reference implementations

- Primary low-level reference: `/home/xertrov/src/pag-server/ref-projects/Roo-Code/src/integrations/openai-codex/oauth.ts`
  - implements PKCE verifier/challenge, localhost callback on port `1455`, token exchange, refresh, and account-id extraction.
- Primary runtime reference: `/home/xertrov/src/pag-server/ref-projects/Roo-Code/src/api/providers/openai-codex.ts`
  - routes requests to `https://chatgpt.com/backend-api/codex` and sends `ChatGPT-Account-Id` when available.
- Integration UX reference: `/home/xertrov/src/pag-server/ref-projects/openclaw/src/commands/openai-codex-oauth.ts`
  - shows local-browser plus manual pasteback UX for remote/headless flows.
- Flow notes: `/home/xertrov/src/pag-server/ref-projects/openclaw/docs/concepts/oauth.md`

### nullclaw

- `nullclaw/` was not present locally, so planning relied on `openclaw` and `Roo-Code` instead.

---

## Proposed Architecture

1. Extend provider config with a Codex-specific auth mode and persisted OAuth credentials.
2. Add a new auth helper/module to manage PKCE login, token exchange, refresh, expiry checks, and account-id extraction.
3. Teach provider selection/runtime that a Codex-authenticated provider is usable without a literal `api_key`.
4. Route Codex-targeted models to the ChatGPT Codex backend with OAuth bearer tokens.
5. Expose user-facing login/status/logout/setup commands and document the callback/manual fallback behavior.

---

## Backlog Decomposition

Epic: `P2.M1.E4` - Codex Pro Subscription Support

- `P2.M1.E4.T001` - Extend provider config for OpenAI Codex OAuth credentials
- `P2.M1.E4.T002` - Implement OpenAI Codex OAuth login and refresh flow
- `P2.M1.E4.T003` - Route OpenAI Codex models through ChatGPT subscription runtime
- `P2.M1.E4.T004` - Add Codex login/logout/status commands and setup flow
- `P2.M1.E4.T005` - Cover Codex subscription support with tests and docs

Critical sequencing:
- `T001` before everything else
- `T002` depends on `T001`
- `T003` depends on `T001` and `T002`
- `T004` depends on `T001` and `T002`
- `T005` depends on `T003` and `T004`

---

## Risks To Watch

- OpenAI may change undocumented Codex OAuth or backend endpoints.
- `ChatGPT-Account-Id` extraction may require tolerant parsing from JWT claims.
- Remote/headless login UX must work without assuming localhost callback success.
- Existing OpenAI-compatible API key flows must remain unchanged.
