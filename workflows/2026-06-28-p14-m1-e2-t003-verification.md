# P14.M1.E2.T003 Verification

Task: `P14.M1.E2.T003 Refresh derived policy on config reload`

Final merge commit: `112cad158296021cda35621a57d2ee02af6107a2`

Repair commit: `603b423e64ffb206297da7a67ee7796c2a370ba3`

Workflow runs:

- Implementation / repair: `wf-20260628T132227Z-p14-m1-e2-t003-loader-boundary-review-fix-e7c2a1`
- Codex review: `wf-20260628T134148Z-p14-m1-e2-t003-loader-boundary-codex-review-ce654f` (PASS)

Verification after merge:

- `make test-run ARGS="test daemon 70-76"`: passed, 7 tests run.
- `make test-run ARGS="test config_loader"`: passed, 80 tests run.
- `make test-run ARGS="test command_bridge 104-105"`: passed, 2 tests run.
- `make fmt-check`: passed.
- `make build`: passed.
- `make test`: passed, 4749 tests run.
- `git diff --check`: passed.
- `git status --short --branch`: clean working tree; `master...origin/master [ahead 59]`.

Notes:

- The repair changed daemon SIGHUP/file-watch reloads to use `Config_loader.load_result`
  so malformed `config.json` preserves the current in-memory config and derived policy
  instead of silently falling back to defaults.
- `Config_loader.load` keeps the legacy fallback-to-default behavior for existing callers.
- `Daemon_util.apply_ec_watcher_toggle` centralizes watcher enable/disable refresh logic.
- The repair workflow hit a ccc/pi runner hang after producing a clean commit; the process
  was terminated and the workflow state was recovered. This is a workflow dogfood issue,
  not a Clawq product failure.
