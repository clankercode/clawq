#!/usr/bin/env bash
set -euo pipefail

if ! command -v opam >/dev/null 2>&1; then
  echo "opam is required but not installed."
  echo "On Arch: sudo pacman -S --needed --noconfirm opam"
  exit 1
fi

if [ ! -d "${HOME}/.opam" ]; then
  opam init --disable-sandboxing -y
fi

eval "$(opam env)"

if ! opam switch show 2>/dev/null | grep -q "clawq-5.1"; then
  opam switch create clawq-5.1 ocaml-base-compiler.5.1.1 -y
fi

eval "$(opam env --switch=clawq-5.1)"
opam install -y dune coq.8.19.2 coq-stdlib cmdliner yojson sqlite3 \
  lwt cohttp-lwt-unix conduit-lwt-unix tls-lwt logs fmt \
  mirage-crypto mirage-crypto-rng kdf digestif base64 alcotest

echo "Bootstrap complete. Next:"
echo "  eval \"\$(opam env --switch=clawq-5.1)\""
echo "  dune build"
