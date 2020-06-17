// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --sets exp.partition-arrays.keep-expr "last" --set ana.activated "['base','expRelation']"
int main(void) {
  example1();
  example2();
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
  for(int i = 0; i < 20; i++) {
      arr[i] = val;
  }
  arr[0] = val;

  assert(arr[2] == val);
  assert(arr[10] == val);
}

// ----------------------------------- Example 2 ------------------------------------------------------------------------------

void example2(void) {
  int arr[20];

  for(int i = 0; i < 20; i++)
  {
    arr[i] = 42;
    assert(arr[i] == 42);
    callee(arr);
  }

  assert(arr[0] == 100); //UNKNOWN
  assert(arr[0] == 7); //UNKNOWN
  assert(arr[0] == 42); //UNKNOWN

  assert(arr[7] == 100); //UNKNOWN
  assert(arr[7] == 7); //UNKNOWN
  assert(arr[7] == 42); //UNKNOWN

  assert(arr[20] == 100); //UNKNOWN
  assert(arr[20] == 7); //UNKNOWN
  assert(arr[20] == 42); //UNKNOWN
}

void callee(int* arr) {
  arr[0] = 7;
  assert(arr[0] == 7);
}
