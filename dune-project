(lang dune 1.11)
(name goblint)
; build failed with: Files src/.maingoblint.eobjs/native/mutex.cmx and _opam/lib/ocaml/threads/threads.cmxa both define a module named Mutex
; https://github.com/ocaml/dune/issues/1727
; https://github.com/ocaml/dune/issues/597
; (implicit_transitive_deps false) ; does not help about the pulled-in Mutex from ocaml/threads
(wrapped_executables true) ; prefix compilation unit names; mentioned here: https://github.com/ocaml/ocaml/pull/2218#issuecomment-572043299; doc says it's the default since dune 2.0, but it somehow still fixed the clash

; https://dune.readthedocs.io/en/stable/dune-files.html#generate-opam-files
; (generate_opam_files true)
