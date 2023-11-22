## v2.3.0 (unreleased)
Functionally equivalent to Goblint in SV-COMP 2024.

* Refactor/fix race analysis (#1170, #1198).
* Add library function (#1167, #1174, #1220, #1203, #1205, #1212).
* Refactor/fix `MayPointTo` and `ReachableFrom` queries (#1142, #1176, #1144).
* Add final messages about unsound results (#1190, #1191).

### SV-COMP 2024
* Add termination analysis (#1093).
* Add OOB analysis (#1094, #1197).
* Add memory leak analysis (???, #1246, #1241).
* Improve multi-threaded use-after-free analysis (#1123, ).
* Support MemSafety in SV-COMP (#1201, #1199, #1259, #1262).
* YAML witnesses in SV-COMP mode (#1217, #1226, #1225, #1248).
* YAML witness version 2.0 (#1238, #1240).
* SV-COMP multi-property (#1220, #1228).
* Adapt autotuning (#912, #921, #987, #1214, #1234, #1168).
* Support `alloca` (#1179).
* Fix old thread analysis soundness (#1223, #1230).
* Add library functions (#1242, #1244, #1254, #1239, #1269).
* Fix some region escape unsoundness (#1247).

## v2.2.1
* Bump batteries lower bound to 3.5.0.
* Fix flaky dead code elimination transformation test.

## v2.2.0
* Add `setjmp`/`longjmp` analysis (#887, #970, #1015, #1019).
* Refactor race analysis to lazy distribution (#1084, #1089, #1136, #1016).
* Add thread-unsafe library function call analysis (#723, #1082).
* Add mutex type analysis and mutex API analysis (#800, #839, #1073).
* Add interval set domain and string literals domain (#901, #966, #994, #1048).
* Add affine equalities analysis (#592).
* Add use-after-free analysis (#1050, #1114).
* Add dead code elimination transformation (#850, #979).
* Add taint analysis for partial contexts (#553, #952).
* Add YAML witness validation via unassume (#796, #977, #1044, #1045, #1124).
* Add incremental analysis rename detection (#774, #777).
* Fix address sets unsoundness (#822, #967, #564, #1032, #998, #1031).
* Fix thread escape analysis unsoundness (#939, #984, #1074, #1078).
* Fix many incremental analysis issues (#627, #836, #835, #841, #932, #678, #942, #949, #950, #957, #955, #954, #960, #959, #1004, #558, #1010, #1091).
* Fix server mode for abstract debugging (#983, #990, #997, #1000, #1001, #1013, #1018, #1017, #1026, #1027).
* Add documentation for configuration JSON schema and OCaml API (#999, #1054, #1055, #1053).
* Add many library function specifications (#962, #996, #1028, #1079, #1121, #1135, #1138).
* Add OCaml 5.0 support (#1003, #945, #1162).

## v2.1.0
Functionally equivalent to Goblint in SV-COMP 2023.

* Add automatic configuration tuning (#772).
* Add many library function specifications (#865, #868, #878, #884, #886).
* Reorganize library stubs (#814, #845).
* Add Trace Event Format output to timing (#844).
* Optimize domains for address and path sets (#803, #809).

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
