## v2.0.1
* Fix compilation with z3.

## v2.0.0
Goblint "lean" release after a lot of cleanup.

* Remove unmaintained analyses: OSEK, ARINC, shapes, containment, deadlocksByRaces (#460, #736, #812, #474).
* Add interactive analysis (#391).
* Add server mode (#522).
* Add Compilation Database support (#406, #448).
* Add floating point domain, unrolled array domain and improved struct domains (#734, #761, #577, #419).
* Add static loop unrolling and heap variable unrolling (#563, #722).
* Improve race detection with may-happen-in-parallel analysis (#529, #518, #595).
* Reimplement lockset and deadlock analyses (#659, #662, #650, #655).
* Add pthread extraction to Promela (#220).
* Add location spans to output (#428, #449).
* Improve race reporting (#550, #551).
* Improve dead code reporting (#94, #353, #785).
* Refactor warnings (#55, #783).
* Add JSON schema for configuration (#476, #499).
* Refactor option names (#28, #192, #516, #675).
* Add bash completion (#669).
* Add OCaml 4.13 and 4.14 support, remove OCaml 4.09 support (#503, #672).

## v1.1.1
* Added lower bounds to dependencies (zarith, ocaml-monadic, sha, conf-gmp).
* Fixed ounit2 library for unit tests.
* Disabled kernel regression tests when linux-headers couldn't be downloaded (e.g. in opam-repository opam-ci).
* Fixed dune-site in opam install by requiring dune 2.9.1.

## v1.1.0

Goblint "fat" release containing 4 years of development.
This version is functionally equivalent to Goblint in SV-COMP 2022.
