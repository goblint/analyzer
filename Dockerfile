FROM ocaml/opam:ubuntu
RUN make deps
CMD make