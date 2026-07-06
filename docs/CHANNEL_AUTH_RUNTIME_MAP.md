# ChannelAuth Runtime Mapping (P5.M2.E3)

This note maps the runtime authorization checks in `src/slack_api.ml`,
`src/discord.ml`, and `src/telegram.ml` to the formal surface in
`coq/theories/Clawq/ChannelAuth.v`.

## Runtime-to-model map

| Runtime path | Runtime behavior | Model surface | Decision |
| --- | --- | --- | --- |
| `Slack.is_allowed` | channel allowlist AND user allowlist; wildcard only for singleton `[*]` | `is_allowed`, `slack_allowed` | Keep |
| `Discord.is_allowed` | guild allowlist AND user allowlist; `guild_id=None` only allowed for wildcard guild list | `is_allowed`, `discord_guild_allowed`, `discord_allowed` | Keep |
| `Slack.verify_signature` timestamp gate | accepts request when absolute clock skew `abs(now - ts) <= 300s` | `timestamp_ok`, `abs_diff` | Replace prior one-sided time model with absolute-skew model |
| Telegram pairing session gate (`is_totp_paired`) | paired session accepted strictly before expiry; denied at/after expiry | `pairing_active` | Keep (narrow model) |

## Major mismatches and keep/replace decisions

- Prior freshness model assumed `current_ts >= request_ts` and rejected all future timestamps. Runtime accepts bounded future skew, so the model is replaced with absolute-skew freshness.
- Prior model did not expose Discord's `guild_id=None` behavior explicitly. Added `discord_guild_allowed` and `discord_allowed` to mirror runtime DM handling.
- Telegram pairing is modeled only as expiry-gated authorization (`pairing_active`). TOTP code generation/verification remains a trusted cryptographic boundary.

## Next proof step

- Use the new compositional surfaces (`slack_allowed`, `discord_allowed`, `timestamp_ok`, `pairing_active`) for follow-on lemmas in `P5.M2.E3.T002` and `P5.M2.E3.T003`.
- If extraction is enabled for ChannelAuth functions, extract only these stable decision functions and keep cryptographic verification as trusted runtime code.
