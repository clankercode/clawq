# Docs Notes

- In `docs/src/components/HeroGearBackdrop.astro`, do not render every backdrop algorithm in dev just to support the debug picker.
- Rendering all algorithms at once makes `/` rebuilds jump from fast to multi-second because every generator and SVG path gets recomputed.
- Keep the page on a single rendered algorithm and let the debug picker fetch/swap the selected backdrop instead.

## Formal Verification Docs Maintenance

Data pipeline: `coq/theories/Clawq/*.v` → `docs/src/data/formal_verification.yml` → `docs/src/data/fv-stats.json` → `docs/src/content/docs/formal-verification.mdx`.

**Automated by `make update-fv`** (runs `scripts/update_fv_data.sh`):
- Theorem/lemma counts in YAML (grepped from `.v` files)
- All derived stats in JSON (totals, percentages, verified/in-progress/planned counts)
- Validation that verified-phase YAML counts match actual `.v` file counts
- Hardcoded counts in `.mdx` (ledger-n values, scroll-count N/N labels)

**Full pipeline including Coq proof check**: `make fv-all` (runs `coq-check` + `update-fv` + `verify-report`).

**Manual steps still required when adding/completing a phase**:
- Update `status` field in `docs/src/data/formal_verification.yml` (e.g. `in_progress` → `verified`)
- Update `extracted` field if extraction status changed
- Add new phase entries to `formal_verification.yml` for new Coq modules
- Add/update phase card, ledger row, and module breakdown accordion in `formal-verification.mdx` (structure and prose — counts are patched automatically)

**When to run `make update-fv`**:
- After adding, removing, or modifying any Theorem/Lemma in a `.v` file
- After changing phase status in `formal_verification.yml`
- Before committing FV-related changes

## llms.txt Maintenance

Two files in `public/`:
- `llms.txt` — spec-compliant index (H1, blockquote summary, H2 link-list sections). Follows the llmstxt.org specification: no headings in body content, H2 sections contain only markdown link lists.
- `llms-full.txt` — full self-knowledge reference with every CLI command, config field/default, tool, channel, endpoint, and setup guide. This is the detailed document clawq uses to understand itself.

**When to update `public/llms-full.txt`:**
- Adding, removing, or renaming a CLI command or subcommand (`src/main.ml`, `src/command_bridge.ml`)
- Adding or changing config fields or defaults (`src/runtime_config.ml`, `src/config_loader.ml`)
- Adding, removing, or renaming a built-in tool (`src/tools_builtin.ml`)
- Changing the shell allowlist or security mechanisms (`src/tools_builtin.ml`)
- Adding or changing HTTP gateway endpoints (`src/http_server.ml`)
- Adding or changing a channel implementation (`src/telegram.ml`, `src/discord.ml`, `src/slack.ml`, `src/slack_socket.ml`, etc.)
- Changing tunnel provider support
- Changing any user-facing behavior documented in the file

**When to update `public/llms.txt`:**
- Adding new doc pages (add to the appropriate H2 link-list section)
- Changing the project summary

**How to update:**
- Keep llms-full.txt factual, concise, and oriented toward clawq operating on itself — not a marketing overview.
- Verify defaults against `Runtime_config.default` in `src/runtime_config.ml`.
- Verify tool names and counts against `src/tools_builtin.ml` registrations.
- Verify command names against `src/main.ml` command list.
- Keep llms.txt spec-compliant: no headings in body, H2 sections are link lists only.
