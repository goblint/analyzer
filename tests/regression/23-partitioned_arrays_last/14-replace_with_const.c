// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.base.partition-arrays.keep-expr "last" --enable ana.base.partition-arrays.partition-by-const-on-return --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --set ana.base.privatization none
#include <assert.h>

int main(void) {
  example1();
}

// ----------------------------------- Example 1 ------------------------------------------------------------------------------
void example1() {
  int a[20];
  int b[20];

  init_array(a, 42);

  assert(a[2] == 42);
  assert(a[10] == 42);

  do_first(a);
  assert(a[0] == 3);

  init_array(b,12);
  assert(b[2] == 12);
  assert(b[10] == 12);
}

void do_first(int* arr) {
  int x = arr[0];
  arr[0] = 3;
}

void init_array(int* arr, int val) {
  for(int i = 0; i < 15; i++) {
      arr[i] = val;
  }

  assert(arr[2] == val);
  assert(arr[10] == val);
}
