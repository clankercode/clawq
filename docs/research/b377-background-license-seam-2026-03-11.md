# B377 next slice: background concurrency gate

## Chosen first gated seam

The best first paid seam is background-task concurrency, not the base assistant
or channel access.

Why this seam fits the current code:

- `Background_task.start_queued_with_callback_impl` is the single scheduler
  choke point for queued work.
- Background tasks already distinguish `queued` from `running`, so a free-tier
  cap is explicit instead of silently degrading behavior.
- The capability is advanced and optional. Free users still keep the core
  assistant and can delegate one task at a time.
- The same gate can later drive upgrade UX in `background add`, `status`, or
  daemon notifications without scattering checks through unrelated modules.

This worktree now has the scheduler primitive needed for that seam:

- `Background_task.available_worker_slots`
- `Background_task.queued_tasks_ready_to_start`
- `Background_task.start_queued_with_callback ?max_running_tasks`

The missing piece is just wiring a validated license into `max_running_tasks`.

## Minimal license-token approach

Use an optional top-level config block:

```json
{
  "license": {
    "token": "$CLAWQ_LICENSE_TOKEN"
  }
}
```

Why this shape fits clawq:

- It matches the existing grouped-root config style in `Runtime_config.t`.
- `token` already benefits from existing redaction/encryption behavior because
  config secret handling keys off the `"token"` substring.
- `Config_loader.parse_config` can resolve it with `Secret_store.resolve_secret`
  the same way provider credentials are resolved.

Recommended token format:

- Compact offline-verifiable token, versioned from day one.
- Payload claims: `iss`, `sub`, `tier`, `caps`, `nbf`, `exp`.
- Signature: Ed25519 over the stable ASCII message
  `clawq-license-v1.<payload_base64url>`.
- Verification key: embedded in the binary, not stored in user config.

Recommended minimal capability mapping:

- No token or invalid token: free tier, `max_running_tasks = Some 1`
- Valid token with `background_parallelism` capability: pro tier,
  `max_running_tasks = None`

## Patch-ready next wiring

1. Add `license` to `Runtime_config.t` and `Config_loader.parse_config`.
2. Introduce a small `Feature_gate` or `License_token` module that returns:
   `Free | Pro` plus a denial reason when validation fails.
3. In the daemon background-task loop, pass
   `~max_running_tasks:(Feature_gate.background_worker_limit config)` to
   `Background_task.start_queued_with_callback`.
4. When queued work is held at the free cap, surface an explicit message in the
   CLI and later in `doctor/status`: "free tier allows 1 concurrent background
   task; upgrade for parallel delegation."

## What not to do first

- Do not gate the main assistant, basic tools, or basic channels.
- Do not require online license checks for daemon startup.
- Do not spread ad hoc `if pro then ...` checks through `command_bridge`,
  `task_tree`, and daemon code before the single validation seam exists.
