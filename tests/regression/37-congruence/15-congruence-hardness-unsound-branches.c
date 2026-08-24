// PARAM: --enable ana.int.congruence --enable ana.int.interval
// reduced (via creduce and manually) from sv-benchmarks/c/hardness-nfm22/hardness_codestructure_dependencies_file-70.c

#include <goblint.h>

main() {
  int a;
  unsigned c = 1;
  if (a)
    c = 4;

  // The following condition evaluated to "both branches dead", due to a bug in the congruence domain.
  // --- Even though the condition is true the concrete.
  if (c + (c + 2))
    a = 1;

  // Check that this is reachable.
  // That is, check that not both branches of previous condition are dead.
  __goblint_check(1);
  return 0;
}
