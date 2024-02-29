  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant", "loop_invariant"]' 11-unrolled-loop-invariant.c
  [Error] YAML witnesses are incompatible with syntactic loop unrolling (https://github.com/goblint/analyzer/pull/1370).
  Fatal error: exception Failure("Option error")
  [2]

TODO: Fix loop unrolling with YAML witnesses: https://github.com/goblint/analyzer/pull/1370
