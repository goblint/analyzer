FROM ocaml/opam:ubuntu-21.04-opam AS dev

COPY --chown=opam Makefile make.sh goblint.opam goblint.opam.locked /home/opam/analyzer/
WORKDIR /home/opam/analyzer/

RUN sudo apt-get update && sudo apt-get install -yq libgmp-dev libmpfr-dev
# RUN sudo apt-get update && sudo apt-get install -yq libgmp-dev libmpfr-dev chrpath && sudo rm -rf /var/lib/apt/lists/*
RUN make setup

COPY --chown=opam . /home/opam/analyzer
RUN make
# RUN make install
# RUN eval $(opam env) && dune build @install && dune install --relocatable --prefix prefix && chrpath -r '$ORIGIN/../share/apron/lib' prefix/bin/goblint

# RUN eval $(opam env) && opam clean -a -c -r --logs && dune clean
# RUN rm -rf /home/opam/opam-repository && rm -rf /home/opam/.opam/repo

# RUN eval $(opam env)
# ENTRYPOINT ["/home/opam/analyzer/prefix/bin/goblint"]


FROM dev AS relocatable

RUN sudo apt-get install chrpath
RUN eval $(opam env) && dune build @install && dune install --relocatable --prefix prefix && chrpath -r '$ORIGIN/../share/apron/lib' prefix/bin/goblint
RUN rm -r prefix/lib
RUN mkdir -p prefix/share/apron/lib/ && cp _opam/share/apron/lib/libapron.so prefix/share/apron/lib/ && cp _opam/share/apron/lib/liboctMPQ.so prefix/share/apron/lib/


FROM ubuntu:21.04

RUN apt-get update && apt-get install -yq libgmp-dev libmpfr-dev && rm -rf /var/lib/apt/lists/*

COPY --from=relocatable /home/opam/analyzer/prefix /opt/goblint/analyzer
# COPY --from=relocatable /home/opam/analyzer/_opam/share/apron /opt/goblint/analyzer/share/apron

# ENV LD_LIBRARY_PATH=/opt/goblint/analyzer/share/apron/lib:$LD_LIBRARY_PATH
ENTRYPOINT ["/opt/goblint/analyzer/bin/goblint"]
