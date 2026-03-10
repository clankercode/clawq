# Revenue Launch Prerequisites (March 2026)

Concrete follow-on from `docs/research/revenue-plan-2026.md`. This note turns the
three monetization paths into launch-enabling work packages, acceptance criteria,
and sequencing.

## Strategic conclusion

The near-term revenue path should not be treated as three equal implementation
tracks. The proper order is:

1. **Path C first** — sponsorships/distribution: cheapest, immediate, validates demand
2. **Path B second** — pro license gating: low-medium engineering, monetizes existing strengths
3. **Path A third** — managed hosting: highest upside, but requires the most packaging and operations

This ordering matches the memo's own evidence: the capability gap is small, but
packaging/distribution are the constraint.

---

## B378 — Distribution + sponsorship launch steps

### Objective

Create the lowest-effort public revenue surface within days, not months.

### Acceptance criteria

- GitHub Sponsors (or equivalent sponsor surface) is visibly enabled from the repo
- README contains a concise sponsorship/pro positioning section
- At least one public roadmap surface exists and is linked from the repo/site
- Release/update cadence is explicit enough that a sponsor can see active stewardship
- At least one outbound announcement/distribution checklist exists for launch posts

### Concrete work packages

1. **Sponsor surface**
   - Add GitHub Sponsors / funding metadata
   - Add README section for sponsorship and "who this is for"
   - Add sponsorship mention to clawq.org if present
2. **Roadmap visibility**
   - Publish a simple public roadmap page or GitHub Project view
   - Link current strategic tracks: webhooks, multi-agent coordination, monetization work
3. **Release discipline**
   - Create a lightweight release/update template
   - Define monthly or biweekly cadence for updates
4. **Distribution checklist**
   - Draft launch post targets: HN Show, r/selfhosted, r/LocalLLaMA, dev communities
   - Define proof points to mention: formal verification, 17+ channels, background delegation, planning pipelines

### Why first

This is the only path that can begin generating signal immediately without adding
substantial product machinery.

---

## B377 — Pro license gating

### Objective

Monetize existing advanced capability with the least architectural distortion.

### Recommended minimal licensable slice

Do **not** begin by gating the broadest or most foundational features. That tends
to create product resentment and engineering mess. Start with features that are
already advanced, differentiated, and optional:

1. **Advanced planning pipeline options**
   - custom reviewer/coder model selection
   - saved pipeline templates
   - higher review iteration limits / richer orchestration
2. **Parallel background delegation beyond a free cap**
   - free: 1 concurrent background worker
   - pro: higher or unlimited concurrent worker cap
3. **Curated MCP registry / easy setup bundle**
   - free: manual MCP config
   - pro: curated one-command registry/setup

Avoid gating the basic assistant, basic tools, or basic channels. That would be a
strategic own-goal.

### Acceptance criteria

- A single license-validation seam exists and is testable
- Feature gates are centralized rather than scattered ad hoc
- Free-tier behavior is explicit and not silently degraded
- Upgrade messaging is clear when a gated feature is attempted
- At least 2-3 premium features are enabled behind the same gating system

### Concrete work packages

1. **License infrastructure**
   - signed local-verifiable license token format
   - config storage and validation path
2. **Gate-check API**
   - one internal module for capability checks (`Feature_gate` or similar)
   - auditability for why access was granted/denied
3. **First gated features**
   - pipeline templates / advanced plan options
   - concurrent background task cap
   - curated MCP registry
4. **Upgrade UX**
   - `clawq doctor/status` hints
   - command-level message when pro-only capability is requested

### Why second

This is the cleanest route to paid conversion from existing product strength with
relatively modest implementation cost.

---

## B376 — Managed hosting plan

### Objective

Package clawq for users who want outcomes, not infrastructure.

### Strategic warning

Managed hosting is attractive, but it is the easiest path to underestimate. It is
not merely "run today's daemon for other people"; it requires user isolation,
onboarding, support surfaces, and billing/operations discipline.

### Acceptance criteria

- A credible hosted MVP exists for 5-10 beta users
- Per-user isolation is explicit and testable
- Users can configure channels and keys without shell access
- Service lifecycle and updates are operator-friendly
- Billing or at least beta-user enrollment flow exists

### Concrete work packages

1. **Web configuration/dashboard MVP**
   - port essential config-wizard flows into web UI
   - support API key entry and channel setup
2. **Tenant isolation model**
   - per-user config/data separation
   - resource and session boundary model
   - clear operator model for upgrades/restarts
3. **Hosted onboarding**
   - one-click or guided Telegram/Discord/Slack onboarding
   - tunnel/webhook provisioning path
4. **Billing / enrollment**
   - initially manual beta enrollment is acceptable
   - Stripe can come after tenant model is credible

### Why third

This likely has the highest revenue ceiling, but also the highest support and
engineering burden. It should follow after demand is validated through Paths C and B.

---

## Recommended 90-day order of attack

### Days 1-14
- Launch sponsorship surface
- Publish roadmap visibility
- Commit to release/update cadence
- Draft public positioning copy

### Days 15-45
- Implement license-validation seam
- Add centralized feature gating
- Gate 2-3 advanced features
- Add upgrade messaging and documentation

### Days 45-90
- Build web config/dashboard MVP
- Define tenant isolation model
- Recruit 5-10 hosted beta users
- Only then decide whether Stripe/billing automation is worth immediate effort

---

## Product positioning thesis

Clawq should be sold first as **the self-hostable, operator-grade AI assistant**:
strong delegation, strong tooling, strong channel reach, and serious engineering
standards. The revenue plan should amplify those strengths rather than conceal them
behind artificial scarcity.
