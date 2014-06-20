# set up a docker ubuntu image
sudo docker run -i -t ubuntu /bin/bash
# inside docker container now!
sudo apt-get update -qq
# needed for goblint
sudo apt-get install -yy --fix-missing make m4 patch autoconf git ruby code2html ocaml opam
# needed for g2html.jar (jdk optional)
sudo apt-get install -yy --fix-missing graphviz openjdk-7-jre openjdk-7-jdk openjdk-7-jre-libs
# compile goblint
git clone https://github.com/goblint/analyzer.git
pushd analyzer
./make.sh
make jar
