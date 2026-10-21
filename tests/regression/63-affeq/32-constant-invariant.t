The exported equality must agree with the value established by the assignment.
  $ goblint --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable witness.invariant.other --disable ana.base.invariant.enabled 32-constant-invariant.c > /dev/null
  $ yamlWitnessStrip < witness.yml | grep 'value:'
        value: (long long )n + 5LL == (long long )x

The dense implementation must export the same equality.
  $ goblint --set ana.activated[+] affeq --disable ana.affeq.sparse --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable witness.invariant.other --disable ana.base.invariant.enabled 32-constant-invariant.c > /dev/null
  $ yamlWitnessStrip < witness.yml | grep 'value:'
        value: (long long )n + 5LL == (long long )x
