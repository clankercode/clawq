# Clawq — The Formal AI Assistant

npm package for installing the `clawq` binary.

See [clawq.org](https://clawq.org) for documentation and [github.com/clankercode/clawq](https://github.com/clankercode/clawq) for source.

## Installation

```
npm install -g @clawq/clawq
```

The current published package is built from the Linux release binary. Use the
source build path on other platforms until platform-specific packages are
available.

Verify the install:

```
clawq version
```

Then run the first-time setup wizard:

```
clawq onboard
```

The checked-in package manifest is a release template. The release workflow
sets the published package version from the git tag and publishes the package
from tagged Clawq releases. Source builds and contributor setup are documented
at [clawq.org/development](https://clawq.org/development).

## License

Unlicense OR CC0-1.0
