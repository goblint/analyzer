# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-22.04-opam AS dev
# make opam 2.2 default for make setup
RUN sudo ln -sf /usr/bin/opam-2.2 /usr/bin/opam

# TODO: use opam depext
RUN sudo apt-get update \
    && sudo apt-get install -y libgmp-dev libmpfr-dev m4 autoconf gcc-multilib pkg-config ruby gem curl python3-pygments graphviz \
    && sudo rm -rf /var/lib/apt/lists/*

RUN sudo gem install parallel os

# remove default Docker <docker@example.com> git credentials added by opam base image: https://github.com/avsm/ocaml-dockerfile/blob/f184554282a3836bf3f1c34d20e77d0530f8349d/src-opam/dockerfile_linux.ml#L24-L28
# this prevents devcontainer from using outside git credentials: https://code.visualstudio.com/docs/remote/containers#_sharing-git-credentials-with-your-container
RUN rm ~/.gitconfig

# otherwise perl (not ruby!) complains during regression testing
ENV LC_ALL=C.UTF-8

USER opam

# update local opam repository because base image may be outdated
RUN cd /home/opam/opam-repository \
    && git pull origin master \
    && opam update -y

# copy only files for make setup to cache docker layers without code changes
COPY --chown=opam Makefile make.sh goblint.opam goblint.opam.locked /home/opam/docker/analyzer/

RUN cd /home/opam/docker/analyzer \
    && OPAMWITHTEST=true OPAMWITHDOC=true OPAMWITHDEVSETUP=true make setup
