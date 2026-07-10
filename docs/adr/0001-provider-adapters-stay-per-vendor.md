# 1. Provider adapters stay per-vendor, not unified by wire format

Date: 2026-07-08
Status: Accepted

## Context

An architecture review proposed cutting the provider seam on **wire format**
rather than vendor: Anthropic, MiniMax, xiaomi, and zai-anthropic all speak the
Anthropic Messages wire format, so the review suggested one Anthropic-wire codec
parameterised by a small per-vendor quirk record, replacing the separate
`provider_anthropic.ml` / `provider_minimax.ml` adapters.

Two facts complicate that:

1. **Routing already collapses the easy cases.** `Provider_routing.detect_kind`
   maps `xiaomi` and `zai_anthropic` to the `Anthropic` kind — they already run
   through `provider_anthropic.ml`. Only **MiniMax** has a separate adapter.

2. **The remaining duplication is not byte-identical.** `provider_anthropic.ml`
   and `provider_minimax.ml` diverge on the LLM inference path:
   - `messages_to_anthropic_json`: MiniMax passes `~strict_pairing:true` (B644);
     Anthropic does not.
   - `parse`: Anthropic sets `provider_response_items_json = None` when there are
     no tool blocks; MiniMax sets `Some body` unconditionally.
   - MiniMax has an inline HTTP-500 / code-1234 retry (B646), a different
     `base_url`, an `api_model_name` casing map, and its own lifted
     `stream_state` / `process_sse_event` (which `provider_anthropic.ml` inlines).

## Decision

Keep the per-vendor provider adapters. Do **not** unify them by wire format as a
speculative refactor.

If unification is pursued later, it must (a) capture every divergence as an
explicit quirk (`strict_pairing`, `provider_items_json` policy, `base_url`,
`name_map`, `retry_policy`, `error_label`, streaming decoder) and (b) be verified
against **real** provider inference, not only the synthetic-event unit tests. The
test suite exercises provider parsing/streaming with hand-written event payloads
only; it cannot catch a quirk that silently changes real tool-calling behaviour.

## Consequences

- Some duplicated codec/streaming code remains between Anthropic and MiniMax.
  That is the accepted cost of not risking silent inference regressions.
- A future architecture review that re-suggests "unify the providers by wire
  format" should read this ADR first: the win is real but small (one adapter),
  and the risk lives on an inference path that unit tests do not cover.
- `tools_to_anthropic_json` is the one genuinely-identical helper; factoring only
  that is safe but low-value on its own.
