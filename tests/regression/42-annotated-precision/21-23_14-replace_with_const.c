// PARAM: --set ana.base.arrays.domain partitioned --set ana.base.partition-arrays.keep-expr "last" --enable ana.base.partition-arrays.partition-by-const-on-return --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include <assert.h>

void example1() __attribute__((goblint_precision("no-def_exc","interval")));
void init_array(int* arr, int val) __attribute__((goblint_precision("no-def_exc","interval")));

int main(void) {
  example1();
}

// ----------------------------------- Example 1 ------------------------------------------------------------------------------
void example1() {
  int a[20];
  int b[20];

  init_array(a, 42);

  __goblint_check(a[2] == 42);
  __goblint_check(a[10] == 42);

  do_first(a);
  __goblint_check(a[0] == 3);

  init_array(b,12);
  __goblint_check(b[2] == 12);
  __goblint_check(b[10] == 12);
}

void do_first(int* arr) {
  int x = arr[0];
  arr[0] = 3;
}

void init_array(int* arr, int val) {
  for(int i = 0; i < 15; i++) {
      arr[i] = val;
  }

  __goblint_check(arr[2] == val);
  __goblint_check(arr[10] == val);
}
