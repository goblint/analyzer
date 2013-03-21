# print all states the parser goes through
#export OCAMLRUNPARAM='p'
bin=src/spec/spec.native
spec=${1-"src/spec/file.spec"}
ocamlbuild -no-links $bin && (./_build/$bin $spec || (echo "$spec failed, running interactive now..."; rlwrap ./_build/$bin))
