# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-24.04-opam
# make opam 2.4 default for make setup
RUN sudo ln -sf /usr/bin/opam-2.4 /usr/bin/opam

RUN sudo apt-get update \
    && sudo apt-get install -y gcc-multilib chrpath
# update local opam repository because base image may be outdated
RUN cd /home/opam/opam-repository \
    && git pull origin master \
    && opam update -y

# copy only files for make setup to cache docker layers without code changes
COPY --chown=opam Makefile make.sh goblint.opam goblint.opam.locked /home/opam/goblint/
WORKDIR /home/opam/goblint/
# unsafe-yes to allow opam to install depexts via apt unattended
RUN OPAMCONFIRMLEVEL=unsafe-yes make setup

# copy the rest
COPY --chown=opam . /home/opam/goblint
# TODO: move up
RUN sudo apt-get install -y wget zip
RUN ./scripts/sv-comp/archive.sh
