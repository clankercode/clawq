# Clawq Revenue Plan: Paths to $1,500/year (March 2026)

Planning memo identifying realistic monetization paths for clawq based on current
capabilities and near-term roadmap.

## Executive Summary

Clawq is a self-hosted, formally verified AI assistant runtime with 17+ channel
integrations, sandboxed tools, background coding delegation, planning pipelines,
and MCP support. The $1,500/year target is achievable through three complementary
paths: (A) a managed hosting tier for non-technical users, (B) a pro license for
power users wanting premium features, and (C) sponsorships plus bounty income from
the open-source community. Each path targets a different customer profile and can
operate independently.

---

## Path A: Managed Hosting (Target: $900/year from ~6 users)

### What it is

A hosted clawq instance where users bring their own API keys but skip all
self-hosting work. Clawq handles the daemon, tunnel, channel wiring, updates,
and uptime.

### Customer profile

- Solo developers or small teams who want a personal AI assistant on
  Telegram/Discord/Slack but don't want to run infrastructure.
- Non-technical power users who are comfortable with chat interfaces but not
  with OCaml builds, opam switches, or daemon supervision.

### Minimum paid-worthy feature set

1. One-click channel onboarding (Telegram, Discord, Slack at minimum).
2. Web dashboard for config (config wizard already exists as TUI; needs web
   equivalent or the existing web UI expanded).
3. Automatic updates and daemon uptime (service management already done).
4. Managed tunnel (Cloudflare tunnel infra already built).
5. Bring-your-own-key model — user provides their OpenRouter/Anthropic/OpenAI
   key; hosting fee covers infrastructure only.

### Pricing sketch

- **Starter**: $12/month ($144/year) — 1 channel, 1 session, basic tools.
- **Standard**: $25/month ($300/year) — 3 channels, unlimited sessions, all
  tools, background tasks, planning pipelines.
- Breakeven at ~6 Standard users for the $1,500 target.
- Hosting cost per user: minimal (SQLite per user, lightweight daemon, ~128MB
  RAM idle).

### Distribution

- Landing page at clawq.org with "Try Hosted" CTA.
- Telegram bot directory / Discord bot listings.
- Dev community posts (HN Show, r/selfhosted, r/LocalLLaMA).

### Roadmap items that directly support this

- Web UI improvements (already have streaming web chat).
- Config wizard web port (TUI wizard exists, web form is incremental).
- Multi-tenant daemon mode (currently single-user; needs user isolation layer).
- Billing integration (Stripe, minimal).

---

## Path B: Pro Self-Hosted License (Target: $600/year from ~8 users)

### What it is

The core runtime stays open source. A pro license unlocks advanced features
that matter to power users and small teams.

### Customer profile

- Developers running clawq on their own hardware who want the full feature set.
- Teams using clawq for internal automation, code review delegation, or
  scheduled agent tasks.

### Minimum paid-worthy feature set (pro-gated)

1. **Multi-agent planning pipelines** — the `plan start` multi-stage pipeline
   (planner → reviewer → coder → code-reviewer) is a strong differentiator.
   Gate advanced pipeline features: configurable reviewer models, parallel
   coder dispatch, pipeline templates.
2. **Background task parallelism** — free tier: 1 concurrent background task;
   pro: unlimited concurrent delegation across runners (Codex, Claude, Kimi,
   Gemini, Opencode, Cursor).
3. **Priority MCP tool marketplace** — curated MCP server configs for common
   integrations (GitHub Issues, Linear, Notion, Jira). Free tier gets manual
   `mcp_servers.json`; pro gets one-line setup.
4. **Audit chain signing + export** — already built (`audit verify`, `audit
   export`). Gate the signing and automated retention/export behind pro.
5. **Advanced channel features** — multi-account Telegram, cross-channel
   message forwarding, agent routing bindings with pattern matching.

### Pricing sketch

- **Pro**: $8/month ($96/year) — all gated features, priority support channel.
- 8 users at $8/month meets the $600 subtarget; combined with Path A reaches
  $1,500.
- Annual discount: $75/year ($6.25/month equivalent).

### Distribution

- GitHub repo README badge ("Pro features available").
- `clawq doctor` and `clawq status` surface upgrade hints when pro features
  are attempted.
- Dev community presence (same channels as Path A).

### Roadmap items that directly support this

- License key validation (lightweight; can be a signed JWT checked locally).
- Feature gating infrastructure (config flag + license check at feature entry
  points).
- Pipeline template system for planning pipelines.
- MCP server config registry (curated JSON index).

---

## Path C: Sponsorships + Bounties (Target: $300-500/year)

### What it is

GitHub Sponsors, Open Collective, and bounty platforms for specific feature
development.

### Customer profile

- Open source enthusiasts who use clawq and want to see it develop.
- Companies using clawq internally who sponsor for goodwill and influence on
  roadmap.
- Users who want specific integrations built (new channels, tools, providers).

### Pricing sketch

- GitHub Sponsors tiers: $5, $15, $50/month.
- Bounty income on specific issues: $25-200 per feature.
- Realistic baseline: 3-5 sponsors at $5-15/month = $180-900/year.

### Distribution

- GitHub Sponsors button on repo.
- Sponsor acknowledgment in README and web UI about page.
- Bounty tags on GitHub issues.

### Roadmap items that directly support this

- Public roadmap visibility (GitHub Projects or similar).
- Contributor documentation improvements.
- Regular release cadence to maintain sponsor confidence.

---

## Feature Prioritization for Revenue

Ranked by revenue impact per engineering effort:

| Priority | Feature | Supports Path | Effort | Revenue Impact |
|----------|---------|---------------|--------|----------------|
| 1 | Web config dashboard | A, B | Medium | High — removes biggest onboarding barrier for hosted |
| 2 | License key gating | B | Low | High — enables entire pro tier |
| 3 | Multi-tenant isolation | A | Medium | High — required for hosted offering |
| 4 | GitHub Sponsors setup | C | Low | Low-Medium — passive income baseline |
| 5 | MCP server registry | B | Low-Medium | Medium — clear pro differentiator |
| 6 | Pipeline templates | B | Medium | Medium — makes planning pipelines more accessible |
| 7 | Stripe billing | A | Low-Medium | High — required for hosted, but only after tenants work |

## Current Roadmap Items Most Directly Supporting Monetization

From the existing codebase and feature set:

1. **Background task system** (done) — delegation to Codex/Claude/Kimi/etc is a
   strong unique selling point. Pipeline orchestration on top is even stronger.
2. **Planning pipelines** (done) — multi-stage planner→reviewer→coder→reviewer
   is production-grade and directly monetizable.
3. **Channel breadth** (done) — 17+ channels means users can adopt without
   switching their existing workflow.
4. **Formal verification story** (done) — differentiator for security-conscious
   buyers, especially in enterprise/pro positioning.
5. **Tunnel management** (done) — removes a major pain point for self-hosters
   who want external channel webhooks.
6. **Web UI + streaming chat** (done) — foundation for hosted tier's user
   interface.
7. **Config wizard** (done, TUI) — needs web port for hosted tier.
8. **Service management** (done) — `clawq service start/stop/restart` plus
   graceful restart already handles daemon lifecycle.

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Low conversion on hosted tier | Keep costs minimal (single VPS, SQLite per user); break even at 2-3 users |
| Pro features feel insufficient | Start with pipeline gating + parallel tasks; add features based on demand |
| Sponsor fatigue | Tie sponsorship to visible roadmap delivery; monthly update posts |
| Competing free alternatives | Lean into formal verification, channel breadth, and self-hosted control as differentiators |
| Support burden on hosted | Automate onboarding; `clawq doctor` catches most config issues; limit to 20-30 hosted users initially |

## Timeline Sketch

- **Month 1**: GitHub Sponsors setup, license key infrastructure, feature gating
  for 2-3 pro features.
- **Month 2**: Web config dashboard MVP, multi-tenant daemon prototype.
- **Month 3**: Hosted tier soft launch (5-10 beta users), Stripe integration.
- **Month 4+**: Iterate based on conversion data; expand pro feature set.

## Bottom Line

$1,500/year is conservative and achievable with 6-8 paying users across hosted
and pro tiers, supplemented by sponsorships. The existing feature set (background
delegation, planning pipelines, 17+ channels, formal verification, tunnels, web
UI) is already substantial — the gap is packaging and distribution, not core
product capability.
