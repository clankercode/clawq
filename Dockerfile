# Stage 1: Build
FROM ocaml/opam:ubuntu-24.04-ocaml-5.1 AS builder

RUN sudo apt-get update && \
    sudo apt-get install -y libsqlite3-dev libgmp-dev pkg-config && \
    sudo rm -rf /var/lib/apt/lists/*

WORKDIR /home/opam/clawq
COPY --chown=opam:opam . .

RUN opam install . --deps-only -y
RUN opam exec -- dune build --release

# Stage 2: Runtime
FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends libsqlite3-0 libgmp10 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /home/opam/clawq/_build/default/src/main.exe /usr/local/bin/clawq

EXPOSE 13451
ENTRYPOINT ["/usr/local/bin/clawq"]
