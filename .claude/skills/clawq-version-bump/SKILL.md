---
name: clawq-version-bump
description: Bump clawq version, commit, tag, push, and create a GitHub release
disable-model-invocation: true
---

# clawq Version Bump

Bump the version, sync all files, build, test, tag, push, and create a draft GitHub release.

## Quick Start

```
/clawq-version-bump 0.2.0
```

## Instructions

The version argument is passed as `$ARGUMENTS`. It must be a valid semver (X.Y.Z).

### 1. Pre-flight

- Verify the git working tree is clean (`git status --porcelain` is empty)
- Validate `$ARGUMENTS` matches `^[0-9]+\.[0-9]+\.[0-9]+$`
- Confirm you are on the `master` branch (or ask user to confirm if on another branch)
- Read the current `VERSION` file to confirm the bump direction makes sense

### 2. Bump version

- Write the new version to the `VERSION` file (single line, no trailing content)
- Run `make sync-version` to propagate to all files (Coq, docs, WASM, tests) and re-extract

### 3. Build and test

- Run `make build` to verify the build succeeds with the new version
- Run `make test-nocontainer` to verify tests pass
- Run `make fmt-check` to verify formatting

### 4. Commit

- Stage all changed files explicitly (do not use `git add -A`):
  - `VERSION`
  - `coq/theories/Clawq/Cli.v`
  - `coq/theories/Clawq/McpFraming.v`
  - `src/extracted/clawq_core.ml`
  - `src/extracted/clawq_core.mli`
  - `src/main_wasm.ml`
  - `docs/package.json`
  - `docs/src/components/Sidebar.astro`
  - `scripts/wasm_templates/IDENTITY.md`
  - `test/test_command_bridge.ml`
  - `test/test_clawq_core.ml`
- Commit with message: `Release v$ARGUMENTS`

### 5. Tag, release, and push

- Create an annotated tag: `git tag -a v$ARGUMENTS -m "Release v$ARGUMENTS"`
- Generate a changelog: `git log --pretty=format:'- %s (%h)' PREV_TAG..v$ARGUMENTS` (where PREV_TAG is the most recent previous `v*` tag, or use `--root` if none)
- Create a draft release (before pushing, so CI can upload artifacts to it):
  ```
  gh release create v$ARGUMENTS --draft --title "v$ARGUMENTS" --notes "$CHANGELOG" --target $(git rev-parse HEAD)
  ```
- Push the commit and tag: `git push origin HEAD && git push origin v$ARGUMENTS`
- CI will automatically build and upload binary artifacts to the release

### 6. Post-release

- Inform the user that:
  - The release is in **draft** state — they should review and undraft when ready
  - CI is building artifacts that will be attached to the release automatically
  - The release URL is provided for review

## Verification

- `make sync-version-check` passes
- `make build` succeeds
- `make test-nocontainer` passes
- `git tag -l 'v*'` shows the new tag
- `gh release view v$ARGUMENTS` shows the draft release

## Guidelines

- Never skip tests or formatting checks
- Always create annotated tags (not lightweight)
- Always create releases as drafts — let the user undraft
- If any step fails, stop and report the error; do not continue
- Do not amend commits or force-push
