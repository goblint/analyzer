FROM ocaml/opam2
SHELL ["/bin/bash", "--login", "-c"]
# {ruby, gem, locale} needed for `make test`
RUN sudo apt-get update && sudo apt-get install -yq m4 ruby
RUN sudo gem install parallel
RUN echo "export LC_ALL=C.UTF-8; export LANG=en_US.UTF-8; export LANGUAGE=en_US.UTF-8" >> /home/opam/.bashrc
COPY . /home/opam/analyzer
WORKDIR /home/opam/analyzer
# ugh, this takes forever... https://github.com/docker/docker/issues/6119
RUN sudo chown -R opam .
# replace with the following once Docker Cloud has version 17.09 (currently 17.06)
# COPY --chown=opam . /home/opam/analyzer
RUN opam switch 4.07
RUN make deps > /dev/null
RUN make
CMD ./goblint
