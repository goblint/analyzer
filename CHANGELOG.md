## v2.7.0 Bamboozled Buffalo (unreleased)
Functionally equivalent to Goblint in SV-COMP 2026.

* Add sequential portfolio for SV-COMP (#1845, #1867, #1877).
* Add struct bitfield support (#1739, #1823).
* Improve bitwise operations for integer domains (#1739).
* Reimplement HTML output in OCaml (#1752).
* Remove YAML witness version 0.1 support (#1812, #1817, #1852, #1853, #1855).
* Fix incorrect invariants in witnesses (#1818, #1876).
* Simplify relational invariants in witnesses (#1826, #1871, #1873).
* Fix argument types in Goblint stubs (#1684, #1814, #1779, #1820).

## v2.6.0 Awkward Aardvark
* Add division by zero analysis (#1764).
* Add bitfield domain (#1623).
* Add weakly-relational C-2PO pointer analysis (#1485).
* Add widening delay (#1358, #1442, #1483).
* Add narrowing of globals to top-down solver (#1636).
* Add weak dependencies to top-down solver (#1746, #1747).
* Add YAML ghost witness generation (#1394).
* Remove GraphML witness generation (#1732, #1733, #1738).
* Use C standard option for preprocessing (#1807).
* Add bash completion for array options (#1670, #1705, #1750).
* Make `malloc(0)` semantics configurable (#1418, #1777).
* Update path-sensitive analyses (#1785, #1791, #1792).
* Fix evaluation of library function arguments (#1758, #1761).
* Optimize affine equalities analysis using sparse matrices (#1459, #1625).
* Prepare for parallelism (#1708, #1744, #1748, #1781, #1790).

## v2.5.0
Functionally equivalent to Goblint in SV-COMP 2025.

* Add 32bit vs 64bit architecture support (#54, #1574).
* Add per-function context gas analysis (#1569, #1570, #1598).
* Adapt automatic static loop unrolling (#1516, #1582, #1583, #1584, #1590, #1595, #1599).
* Adapt automatic configuration tuning (#1450, #1612, #1181, #1604).
* Simplify non-relational integer invariants in witnesses (#1517).
* Fix excessive hash collisions (#1594, #1602).
* Clean up various code (#1095, #1523, #1554, #1575, #1588, #1597, #1614).

## v2.4.0
* Remove unmaintained analyses: spec, file (#1281).
* Add linear two-variable equalities analysis (#1297, #1412, #1466).
* Add callstring, loopfree callstring and context gas analyses (#1038, #1340, #1379, #1427, #1439).
* Add non-relational thread-modular value analyses with thread IDs (#1366, #1398, #1399).
* Add NULL byte array domain (#1076).
* Fix spurious overflow warnings from internal evaluations (#1406, #1411, #1511).
* Refactor non-definite mutex handling to fix unsoundness (#1430, #1500, #1503, #1409).
* Fix non-relational thread-modular value analysis unsoundness with ambiguous points-to sets (#1457, #1458).
* Fix mutex type analysis unsoundness and enable it by default (#1414, #1416, #1510).
* Add points-to set refinement on mutex path splitting (#1287, #1343, #1374, #1396, #1407).
* Improve narrowing operators (#1502, #1540, #1543).
* Extract automatic configuration tuning for soundness (#1469).
* Fix many locations in witnesses (#1355, #1372, #1400, #1403).
* Improve output readability (#1294, #1312, #1405, #1497).
* Refactor logging (#1117).
* Modernize all library function specifications (#1029, #688, #1174, #1289, #1447, #1487).
* Remove OCaml 4.10, 4.11, 4.12 and 4.13 support (#1448).

## v2.3.0
Functionally equivalent to Goblint in SV-COMP 2024.

* Add termination analysis for loops (#1093).
* Add memory out-of-bounds analysis (#1094, #1197).
* Add memory leak analysis (#1127, #1241, #1246).
* Add SV-COMP `termination`, `valid-memsafety` and `valid-memcleanup` properties support (#1220, #1228, #1201, #1199, #1259, #1262).
* Add YAML witness version 2.0 support (#1238, #1240, #1217, #1226, #1225, #1248).
* Add final warnings about unsound results (#1190, #1191).
* Add many library function specifications (#1167, #1174, #1203, #1205, #1212, #1220, #1239, #1242, #1244, #1254, #1269).
* Adapt automatic configuration tuning (#912, #921, #987, #1168, #1214, #1234).

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
