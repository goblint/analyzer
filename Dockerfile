FROM ocaml/opam:ubuntu
RUN sudo apt-get install -yq ruby
COPY / analyzer
WORKDIR analyzer
# ugh... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam scripts src
RUN sudo chown opam .
RUN make deps
CMD make