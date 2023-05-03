Check that assert transform is not allowed to happen after dead code removal
  $ ./transform.sh --stderr remove_dead_code assert -- 01-empty.c
  Option failure: trans.activated: the 'assert' transform may not occur after the 'remove_dead_code' transform
  Fatal error: exception Failure("Option failure: trans.activated: the 'assert' transform may not occur after the 'remove_dead_code' transform")
  [2]
