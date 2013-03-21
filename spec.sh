bin=src/spec/spec.native
ocamlbuild -no-links ${bin} && (./_build/${bin} src/spec/file.spec || echo "file.spec failed, running interactive now..."; rlwrap ./_build/${bin})
