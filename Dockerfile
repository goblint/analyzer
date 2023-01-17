# dev stage: development environment with opam, ocaml and dependencies

# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-22.04-opam AS dev
# make opam 2.1 default for make setup
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam

# copy only files for make setup to cache docker layers without code changes
COPY --chown=opam Makefile make.sh goblint.opam goblint.opam.locked /home/opam/analyzer/
WORKDIR /home/opam/analyzer/
# TODO: use opam depext
RUN sudo apt-get update \
    && sudo apt-get install -y libgmp-dev libmpfr-dev pkg-config autoconf gcc-multilib
# update local opam repository because base image may be outdated
RUN cd /home/opam/opam-repository \
    && git pull origin master \
    && opam update
RUN make setup
# copy the rest
COPY --chown=opam . /home/opam/analyzer
RUN make


# relocatable stage: relocatable installation with only runtime dependencies
FROM dev AS relocatable

RUN sudo apt-get install -y chrpath
RUN make relocatable


# final stage: minimal run environment for small docker image
FROM ubuntu:22.04

# cannot use opam depext because no opam here, also additional preprocessing/header dependencies
# libgmp for zarith, libmpfr for apron, cpp for preprocessing, libc6 for pthread.h, libgcc for stddef.h
RUN apt-get update \
    && apt-get install -y libgmp-dev libmpfr-dev cpp libc6-dev libc6-dev-i386 libgcc-11-dev lib32gcc-11-dev \
    && rm -rf /var/lib/apt/lists/*
COPY --from=relocatable /home/opam/analyzer/relocatable /opt/goblint/analyzer
ENTRYPOINT ["/opt/goblint/analyzer/bin/goblint"]
