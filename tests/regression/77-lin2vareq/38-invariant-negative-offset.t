At the join, y = x - 1. The offset is a mathematical integer, even though
both variables are unsigned.

  $ goblint --set ana.activated[+] lin2vareq --enable ana.sv-comp.functions --disable ana.base.invariant.enabled --disable ana.relation.invariant.one-var --enable witness.yaml.enabled 38-invariant-negative-offset.c > /dev/null
  $ yamlWitnessStrip < witness.yml | grep 'value:'
        value: (long long )x == (long long )y + 1LL
