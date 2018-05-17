FROM ocaml/opam2-staging:ubuntu-18.04-ocaml-4.06
SHELL ["/bin/bash", "--login", "-c"]
RUN sudo apt-get update && sudo apt-get install -yq m4 ruby
COPY --chown=opam . /home/opam/analyzer
WORKDIR /home/opam/analyzer
RUN make dep
RUN make
CMD ./goblint --help
