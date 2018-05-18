FROM ocaml/opam2-staging:ubuntu-18.04-ocaml-4.06
SHELL ["/bin/bash", "--login", "-c"]
RUN sudo apt-get update && sudo apt-get install -yq m4 ruby
COPY . /home/opam/analyzer
WORKDIR /home/opam/analyzer
# ugh, this takes forever... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam .
# replace with the following once Docker Cloud has version 17.09 (currently 17.06)
# COPY --chown=opam . /home/opam/analyzer
RUN make dep
RUN make
CMD ./goblint --help
