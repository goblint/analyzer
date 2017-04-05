FROM ocaml/opam:ubuntu
COPY / analyzer
WORKDIR analyzer
# ugh... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam scripts src
RUN make deps
CMD make