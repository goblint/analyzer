// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.base.privatization none --disable exp.fast_global_inits
// This checks that partitioned arrays and fast_global_inits are no longer incompatible
#include <assert.h>

int global_array[50];
int global_array_multi[50][2][2];

int main(void) {
  for(int i =0; i < 50; i++) {
      __goblint_check(global_array[i] == 0);
      __goblint_check(global_array_multi[i][1][1] == 0);
  }
}
