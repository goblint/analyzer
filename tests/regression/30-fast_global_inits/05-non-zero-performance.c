// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --enable exp.fast_global_inits
#include <assert.h>

int global_array[10000] = {9, 0, 3, 42, 11 }; // All non-specified ones will be zero
int global_array_multi[2][10000] =  {{9, 0, 3, 42, 11}, {9, 0, 3, 42, 11}};  // All non-specified ones will be zero

int main(void) {
  __goblint_check(global_array[0] == 9);  //UNKNOWN
  __goblint_check(global_array[1] == 0);  //UNKNOWN
  __goblint_check(global_array[2] == 3);  //UNKNOWN
  __goblint_check(global_array[3] == 42); //UNKNOWN
  __goblint_check(global_array[4] == 11); //UNKNOWN

  __goblint_check(global_array_multi[0][0] == 9);  //UNKNOWN
  __goblint_check(global_array_multi[0][1] == 0);  //UNKNOWN
  __goblint_check(global_array_multi[0][2] == 3);  //UNKNOWN
  __goblint_check(global_array_multi[0][3] == 42); //UNKNOWN
  __goblint_check(global_array_multi[0][4] == 11); //UNKNOWN


  __goblint_check(global_array_multi[1][0] == 9);  //UNKNOWN
  __goblint_check(global_array_multi[1][1] == 0);  //UNKNOWN
  __goblint_check(global_array_multi[1][2] == 3);  //UNKNOWN
  __goblint_check(global_array_multi[1][3] == 42); //UNKNOWN
  __goblint_check(global_array_multi[1][4] == 11); //UNKNOWN
}
