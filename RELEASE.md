# Release Process

## Tag-based release

Pushing a `v*` tag triggers the `release` workflow, which:

1. **Builds optimized binaries** (speed + size variants, stripped)
2. **Creates a GitHub release** (draft, with auto-generated notes)
3. **Uploads binaries** to the GitHub release
4. **Publishes to npm** as `@clawq/clawq` when `NPM_TOKEN` is configured

```bash
# Example release
git tag v0.1.0
git push origin v0.1.0
```

## npm

The npm package is published automatically from CI when `NPM_TOKEN` is
configured. The package version is derived from the git tag (strips the
leading `v`). If `NPM_TOKEN` is not configured, the release workflow skips npm
publishing after building the GitHub release assets.

The `npm-pkg/` directory contains the package skeleton. `just bi` (or
`scripts/build-install.sh`) builds the release binary, extracts debug symbols,
strips the binary, and places both in `npm-pkg/bin/`.

### Manual npm publish

If needed, you can publish manually:

```bash
just bi
cd npm-pkg
npm version <version>
npm publish --access=public
```

Requires `NPM_TOKEN` to be set or `npm login`.

## Secrets

- `NPM_TOKEN` — npm automation token for `@clawq` org (set in repo secrets)
- `GITHUB_TOKEN` — provided automatically by GitHub Actions
