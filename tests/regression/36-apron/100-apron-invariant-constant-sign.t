Regression test for issue #2159: the affine-equality Apron invariant exporter
must not negate the constant term. Before the fix the exported witness contained
the self-contradictory conjunct "x + 1 == z" alongside "x == z + 1".

  $ goblint --set ana.activated '["base","threadid","mallocWrapper","mutexEvents","mutex","access","affeq","apron"]' --set ana.autotune.enabled true --set ana.autotune.activated '["loopUnrollHeuristic"]' --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled 100-apron-invariant-constant-sign.c > /dev/null 2>&1
  $ grep -c 'type: loop_invariant' witness.yml
  1
  $ tr '\n' ' ' < witness.yml | tr -s ' ' | grep -c 'x + 1LL == (long long )z'
  0
  $ tr '\n' ' ' < witness.yml | tr -s ' ' | grep -c 'x == (long long )z + 1LL'
  1
