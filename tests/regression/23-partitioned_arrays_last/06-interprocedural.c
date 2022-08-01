// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.base.partition-arrays.keep-expr "last" --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

int main(void) {
  example1();
  example2();
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
  for(int i = 0; i < 20; i++) {
      arr[i] = val;
  }
  arr[0] = val;

  __goblint_check(arr[2] == val);
  __goblint_check(arr[10] == val);
}

// ----------------------------------- Example 2 ------------------------------------------------------------------------------

void example2(void) {
  int arr[20];

  for(int i = 0; i < 20; i++)
  {
    arr[i] = 42;
    __goblint_check(arr[i] == 42);
    callee(arr);
  }

  __goblint_check(arr[0] == 100); //UNKNOWN
  __goblint_check(arr[0] == 7); //UNKNOWN
  __goblint_check(arr[0] == 42); //UNKNOWN

  __goblint_check(arr[7] == 100); //UNKNOWN
  __goblint_check(arr[7] == 7); //UNKNOWN
  __goblint_check(arr[7] == 42); //UNKNOWN

  __goblint_check(arr[20] == 100); //UNKNOWN
  __goblint_check(arr[20] == 7); //UNKNOWN
  __goblint_check(arr[20] == 42); //UNKNOWN
}

void callee(int* arr) {
  arr[0] = 7;
  __goblint_check(arr[0] == 7);
}
