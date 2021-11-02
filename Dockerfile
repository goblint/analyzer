# dev stage: development environment with opam, ocaml and dependencies

# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-21.04-opam AS dev
# copy only files for make setup to cache docker layers without code changes
COPY --chown=opam Makefile make.sh goblint.opam goblint.opam.locked /home/opam/analyzer/
WORKDIR /home/opam/analyzer/
# TODO: use opam depext
RUN sudo apt-get update \
    && sudo apt-get install -y libgmp-dev libmpfr-dev
RUN make setup
# copy the rest
COPY --chown=opam . /home/opam/analyzer
RUN make


# relocatable stage: relocatable installation with only runtime dependencies
FROM dev AS relocatable

RUN sudo apt-get install -y chrpath
RUN make relocatable


# final stage: minimal run environment for small docker image
FROM ubuntu:21.04

# cannot use opam depext because no opam here, also additional preprocessing/header dependencies
# libgmp for zarith, libmpfr for apron, cpp for preprocessing, libc6 for pthread.h, libgcc for stddef.h
RUN apt-get update \
    && apt-get install -y libgmp-dev libmpfr-dev cpp libc6-dev libgcc-10-dev \
    && rm -rf /var/lib/apt/lists/*
COPY --from=relocatable /home/opam/analyzer/relocatable /opt/goblint/analyzer
ENTRYPOINT ["/opt/goblint/analyzer/bin/goblint"]
