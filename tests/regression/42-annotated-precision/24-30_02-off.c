// PARAM: --set ana.base.arrays.domain partitioned --disable exp.fast_global_inits --enable annotation.int.enabled --set ana.int.refinement fixpoint
// This checks that partitioned arrays and fast_global_inits are no longer incompatible
#include <goblint.h>

int global_array[50];
int global_array_multi[50][2][2];

int main(void) __attribute__((goblint_precision("no-def_exc","interval")));

int main(void) {
  for(int i =0; i < 50; i++) {
      __goblint_check(global_array[i] == 0);
      __goblint_check(global_array_multi[i][1][1] == 0);
  }
}
