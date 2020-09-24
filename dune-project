(lang dune 2.3)
(name goblint)
; build failed with: Files src/.maingoblint.eobjs/native/mutex.cmx and _opam/lib/ocaml/threads/threads.cmxa both define a module named Mutex
; maybe related: https://github.com/ocaml/dune/issues/1727, https://github.com/ocaml/dune/issues/597
; (implicit_transitive_deps false) ; does not help about the pulled-in Mutex from ocaml/threads
(wrapped_executables true) ; prefix compilation unit names; mentioned here: https://github.com/ocaml/ocaml/pull/2218#issuecomment-572043299; doc says it's the default since dune 2.0, but it somehow still fixes the clash

; https://dune.readthedocs.io/en/stable/dune-files.html#generate-opam-files
(generate_opam_files true)

(source (github goblint/analyzer))
(homepage "https://goblint.in.tum.de")
(documentation "https://goblint.github.io/analyzer")
(authors "Vesal Vojdani, Kalmer Apinis, Ralf Vogler, Michael Schwarz, Julian Erhard")
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Ralf Vogler <ralf.vogler@gmail.com>")
(license MIT)

(package
  (name goblint)
  (synopsis "Static analysis framework for concurrent C")
  (depends
    (ocaml (>= 4.04.1)) ; TODO check with travis env matrix as for goblint-cil
    dune
    zarith_stubs_js ; only needed for js_of_ocaml
    goblint-cil ; TODO no way to define as pin-depends? Used goblint.opam.template to add it for now. https://github.com/ocaml/dune/issues/3231. Alternatively, removing this line and adding cil as a git submodule and `(vendored_dirs cil)` as ./dune also works. This way, no more need to reinstall the pinned cil opam package on changes. However, then cil is cleaned and has to be rebuild together with goblint.
    batteries
    xml-light
    qcheck
    z3
    (ppx_distr_guards (>= 0.2))
    ppx_import
    ppx_deriving
    ppx_deriving_yojson
    ocaml-monadic
    (ounit2 :with-test)
    (odoc :with-doc)
    ; TODO still need the following after switch to dune?
    ocamlbuild
    ocamlfind
  )
)
