# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-21.04-opam AS dev

# TODO: use opam depext
RUN sudo apt-get update \
    && sudo apt-get install -y libgmp-dev libmpfr-dev m4 autoconf pkg-config ruby gem curl
