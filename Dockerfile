FROM ocaml/opam:ubuntu
RUN sudo apt-get install -yq ruby
COPY / analyzer
WORKDIR analyzer
# ugh, this takes forever... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam .
RUN make deps
CMD make