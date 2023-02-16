// PARAM: --enable ana.int.interval_set --set ana.base.arrays.domain partitioned
// This checks that partitioned arrays and fast_global_inits are no longer incompatible
#include <goblint.h>

int global_array[50];
int global_array_multi[50][2][2];

int main(void) {
  for(int i =0; i < 50; i++) {
      __goblint_check(global_array[i] == 0);
      __goblint_check(global_array_multi[i][1][1] == 0);
  }
}
