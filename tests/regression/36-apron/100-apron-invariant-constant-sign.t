Regression test for issue #2159: the affine-equality Apron invariant exporter
must not negate the constant term. Before the fix the exported witness contained
the self-contradictory conjunct "x + 1 == z" alongside "x == z + 1".

  $ goblint --set ana.activated[+] apron --set ana.apron.domain affeq --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box --set sem.int.signed_overflow assume_none 100-apron-invariant-constant-sign.c > /dev/null 2>&1
  $ tr '\n' ' ' < witness.yml | tr -s ' ' | grep -c 'x + 1LL == (long long )z'
  0
