(lang dune 2.9)
; need 2.9 for dune-site to work with opam install: https://github.com/ocaml/dune/issues/4212, https://github.com/ocaml/dune/pull/4645, https://github.com/ocaml/dune/pull/4774
(using dune_site 0.1)
(name goblint)
; build failed with: Files src/.maingoblint.eobjs/native/mutex.cmx and _opam/lib/ocaml/threads/threads.cmxa both define a module named Mutex
; maybe related: https://github.com/ocaml/dune/issues/1727, https://github.com/ocaml/dune/issues/597
; (implicit_transitive_deps false) ; does not help about the pulled-in Mutex from ocaml/threads
(wrapped_executables true) ; prefix compilation unit names; mentioned here: https://github.com/ocaml/ocaml/pull/2218#issuecomment-572043299; doc says it's the default since dune 2.0, but it somehow still fixes the clash

; https://dune.readthedocs.io/en/stable/dune-files.html#generate-opam-files
; goblint.opam is generated on `dune build` (not normal make!) from this file and goblint.opam.template
; also remember to generate/adjust goblint.opam.locked!
(generate_opam_files true)

(source (github goblint/analyzer))
(homepage "https://goblint.in.tum.de")
(documentation "https://goblint.readthedocs.io/en/latest/")
(authors "Vesal Vojdani" "Kalmer Apinis" "Ralf Vogler" "Michael Schwarz" "Julian Erhard" "Simmo Saan") ; TODO: same authors as in CITATION.cff (for future releases)
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Simmo Saan <simmo.saan@gmail.com>" "Ralf Vogler <ralf.vogler@gmail.com>")
(license MIT)

(package
  (name goblint)
  (synopsis "Static analysis framework for C")
  (depends
    (ocaml (>= 4.10))
    (dune (>= 2.9.1)) ; explicit lower bound needed for opam install (https://github.com/ocaml/dune/pull/4806), (lang dune 2.9.1) doesn't do this
    (goblint-cil (>= 1.8.2)) ; TODO no way to define as pin-depends? Used goblint.opam.template to add it for now. https://github.com/ocaml/dune/issues/3231. Alternatively, removing this line and adding cil as a git submodule and `(vendored_dirs cil)` as ./dune also works. This way, no more need to reinstall the pinned cil opam package on changes. However, then cil is cleaned and has to be rebuild together with goblint.
    (batteries (>= 3.4.0))
    (zarith (>= 1.8))
    (yojson (>= 2.0.0))
    qcheck-core
    (ppx_distr_guards (>= 0.2))
    ppx_deriving
    ppx_deriving_hash
    ppx_deriving_yojson
    (ppx_blob (>= 0.6.0))
    (ounit2 :with-test)
    (qcheck-ounit :with-test)
    (odoc :with-doc)
    fpath
    dune-site
    json-data-encoding
    (jsonrpc (>= 1.12))
    (sha (>= 1.12))
    cpu
    arg-complete
    yaml
    uuidm
    (conf-gmp (>= 3)) ; only needed transitively, but they don't have lower bound, which is needed on MacOS
    (conf-ruby :with-test)
    (benchmark :with-test) ; TODO: make this optional somehow, (optional) on bench executable doesn't work
  )
  (depopts
    apron
    z3
  )
  (sites (share includes) (share conf))
)
