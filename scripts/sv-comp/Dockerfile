# stage: download the published WITCH SV-COMP 2027 binaries
FROM ubuntu:26.04 AS witch-download

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates wget unzip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
# https://zenodo.org/records/23473531
RUN wget -O witch.zip https://zenodo.org/api/records/23473531/files/witch.zip/content \
    && echo 'e48aa809ff08ca8088d781974d462ac695fd1ed19396a8472913302d0807d9d0  witch.zip' | sha256sum -c - \
    && unzip -q witch.zip \
    && rm witch.zip \
    && test -x witch/bin/symbiotic

# just -opam tag because make setup will install ocaml compiler
FROM ocaml/opam:ubuntu-26.04-opam AS build
# make opam 2.4 default for make setup
RUN sudo ln -sf /usr/bin/opam-2.4 /usr/bin/opam

RUN sudo apt-get update \
    && sudo apt-get install -y gcc-multilib chrpath wget zip
# update local opam repository because base image may be outdated
RUN cd /home/opam/opam-repository \
    && git pull origin master \
    && opam update -y

# copy only files for make setup to cache docker layers without code changes
COPY --chown=opam make.sh goblint.opam goblint.opam.locked /home/opam/goblint/
WORKDIR /home/opam/goblint/
# unsafe-yes to allow opam to install depexts via apt unattended
RUN OPAMCONFIRMLEVEL=unsafe-yes ./make.sh setup

# copy the rest
COPY --chown=opam . /home/opam/goblint
COPY --from=witch-download --chown=opam /opt/witch/ witch/
RUN ./scripts/sv-comp/archive.sh

FROM ubuntu:26.04 AS smoketest-ubuntu

# cannot use opam depext because no opam here, also additional preprocessing/header dependencies
# libgmp for zarith, libmpfr for apron, cpp for preprocessing, libc6 for pthread.h, libgcc for stddef.h
RUN apt-get update \
    && apt-get install -y unzip libgmp10 libmpfr6 cpp libc6-dev libc6-dev-i386 libgcc-15-dev lib32gcc-15-dev python3 python3-yaml \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root
COPY --from=build /home/opam/goblint/scripts/sv-comp/goblint.zip .
RUN unzip goblint.zip

WORKDIR /root/goblint
RUN ./smoketest.sh

FROM registry.gitlab.com/sosy-lab/benchmarking/competition-scripts/user:2027 AS smoketest-competition

RUN apt-get update \
    && apt-get install -y unzip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root
COPY --from=build /home/opam/goblint/scripts/sv-comp/goblint.zip .
RUN unzip goblint.zip

WORKDIR /root/goblint
RUN ./smoketest.sh

FROM scratch
COPY --from=build /home/opam/goblint/scripts/sv-comp/goblint.zip .
