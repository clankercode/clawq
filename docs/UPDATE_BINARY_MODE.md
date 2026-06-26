# `/update` Binary Mode

`update_clawq` now supports four modes:

- `auto`: prefer git rebuild when a repo is available, then the package manager that installed clawq, then binary download
- `git`: require a repo checkout and run `git pull` + `make build`
- `pkg`: upgrade through the package manager that installed clawq (npm/pnpm/yarn/bun/Homebrew)
- `binary`: require `CLAWQ_UPDATE_BINARY_URL` and replace the current executable directly

The daemon-owned update flow is reachable from chat via `/update` and from the CLI via `clawq update [--mode auto|git|binary|pkg]`.

## Package-manager mode

`pkg` mode (and the `auto` fallback) detects how the running binary was installed by
resolving its real path and matching the install tree, then runs the matching upgrade:

| Manager  | Detected from (real path contains)              | Upgrade command                      |
| -------- | ----------------------------------------------- | ------------------------------------ |
| npm      | `lib/node_modules/` or `node_modules/@clawq`    | `npm install -g @clawq/clawq@latest` |
| pnpm     | `pnpm/` or `node_modules/.pnpm`                 | `pnpm add -g @clawq/clawq@latest`    |
| yarn     | `yarn/global` or `.config/yarn`                 | `yarn global add @clawq/clawq@latest`|
| bun      | `.bun/` or `bun/install/global`                 | `bun add -g @clawq/clawq@latest`     |
| Homebrew | `Cellar/clawq`, `homebrew/`, or `.linuxbrew/`   | `brew upgrade clawq`                 |

Notes:

- Detection only resolves to a manager when its CLI is also on `PATH`; otherwise the update
  falls through to the next `auto` candidate (or reports that `pkg` mode is unavailable).
- Path matching is case-insensitive and separator-agnostic, so the same logic works on
  Linux, macOS, and Windows (Homebrew is excluded on Windows). Windows is not yet a
  supported runtime target, but the code paths are in place.
- After a successful upgrade, clawq re-execs via the stable on-`PATH` symlink (not the
  resolved versioned path), since pnpm and Homebrew replace the versioned store directory.

Current CLI note:

- `clawq update` targets a live daemon over the local gateway.
- If no live daemon is available, the CLI currently warns and exits because the offline fallback path is still a stub.

Binary mode uses this environment variable:

```bash
CLAWQ_UPDATE_BINARY_URL=https://example.invalid/path/to/clawq
```

In binary mode, clawq:

1. downloads to `<current-executable>.download`
2. runs `chmod 755` on the downloaded file
3. moves it over the current executable
4. sends `SIGUSR1` to trigger the normal graceful restart path

Current guidance:

- Use `auto` as the default tool behavior — it picks git, package manager, or binary automatically.
- Use `git` mode for developer checkouts.
- Use `pkg` mode to force a package-manager upgrade (npm/pnpm/yarn/bun/Homebrew installs).
- Use `binary` mode for packaged installs only when a trusted binary URL is configured.
