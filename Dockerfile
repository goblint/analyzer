# default tag 'latest' is Debian Stable with opam switches for OCaml 4.02-4.08 (currently, 1 GB)
# the <DISTRO>-opam images just have opam (ubuntu-19.04-opam is 223 MB)
FROM ocaml/opam2
# To best make use of the build cache, layers should be ordered by frequency of change.
# Here: apt packages, make.sh, linux-headers, opam packages, source code
# {ruby, gem, locale} needed for `make test`
RUN sudo apt-get update && sudo apt-get install -yq m4 libgmp-dev ruby
RUN sudo gem install parallel
RUN opam switch 4.07
# First we only copy files needed for setup. If we added all here, it would invalidate the cache on every change and the following steps would have to be rerun.
COPY --chown=opam make.sh /home/opam/analyzer/
# Change workdir after first copy, otherwise wrong permissions.
WORKDIR /home/opam/analyzer
# Download linux-headers before installing dependencies, so that this can be cached separately.
RUN ./make.sh headers
# dependencies and their locked versions; upgraded versions will only be installed after a change to opam.locked
COPY --chown=opam opam opam.locked /home/opam/analyzer/
# The base image uses a local opam repository which can (did) lag behind the online one. If we upgraded locally, we also want it to work in the container and not wait until the change made it into the base image. Thus, add the online version as default before we update.
RUN opam repository set-url default https://opam.ocaml.org/
# install locked dependencies
RUN ./make.sh deps
# add the rest to the image (~11s), .dockerignore is symlinked to .gitignore
COPY --chown=opam . /home/opam/analyzer
RUN make
# need UTF-8 for test script, image's default is US-ASCII
ENV LC_ALL=C.UTF-8
# RUN make test
CMD ./goblint
