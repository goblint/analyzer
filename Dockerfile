FROM ocaml/opam:ubuntu-17.04_ocaml-4.06.0
RUN sudo apt-get install -yq ruby
COPY / analyzer
WORKDIR analyzer
# ugh, this takes forever... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam .
RUN make setup
CMD make
