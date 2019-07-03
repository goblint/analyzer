# To best make use of the build cache, layers should be ordered by frequency of change.
# Here: apt packages, make.sh, linux-headers, opam packages, source code
FROM ocaml/opam2
# {ruby, gem, locale} needed for `make test`
RUN sudo apt-get update && sudo apt-get install -yq m4 libgmp-dev ruby
RUN sudo gem install parallel
RUN opam switch 4.07
# First we only copy files needed for setup. If we added all here, it would invalidate the cache on every change and the following steps would have to be rerun.
COPY --chown=opam make.sh opam /home/opam/analyzer/
WORKDIR /home/opam/analyzer
RUN ./make.sh headers
RUN ./make.sh deps
# add the rest to the image (~11s), .dockerignore is symlinked to .gitignore
COPY --chown=opam . /home/opam/analyzer
RUN make
# need UTF-8 for test script, image's default is US-ASCII
ENV LC_ALL=C.UTF-8
# RUN make test
CMD ./goblint
