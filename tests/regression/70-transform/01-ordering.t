Suppress backtrace with code locations, especially for CI.
  $ export OCAMLRUNPARAM=b=0

Check that assert transform is not allowed to happen after dead code removal
  $ ./transform.sh --stderr remove_dead_code assert -- 01-empty.c
  [Error] trans.activated: the 'assert' transform may not occur after the 'remove_dead_code' transform
  Fatal error: exception Failure("Option error")
  [2]
