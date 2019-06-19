FROM ocaml/opam2
SHELL ["/bin/bash", "--login", "-c"]
# {ruby, gem, locale} needed for `make test`
RUN sudo apt-get update && sudo apt-get install -yq m4 libgmp-dev ruby
RUN sudo gem install parallel
RUN echo "export LC_ALL=C.UTF-8; export LANG=en_US.UTF-8; export LANGUAGE=en_US.UTF-8" >> /home/opam/.bashrc
# add files to image (~11s), .dockerignore is symlinked to .gitignore
COPY --chown=opam . /home/opam/analyzer
WORKDIR /home/opam/analyzer
RUN opam switch 4.07
RUN make deps > /dev/null
RUN make
CMD ./goblint
