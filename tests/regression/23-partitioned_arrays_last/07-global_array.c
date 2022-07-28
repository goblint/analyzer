// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.base.partition-arrays.keep-expr "last" --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

int global_array[50];

int main(void) {
  some_func();

  int x = global_array[5];
  __goblint_check(x == 0); //UNKNOWN
  __goblint_check(x == 42); //UNKNOWN
}


void some_func(void) {
  global_array[0] = 5;

  for(int i=1; i < 50; i++) {
    global_array[i] = 42;
  }

  int x = global_array[0];
  __goblint_check(x == 42); //UNKNOWN
}
