// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --sets exp.partition-arrays.keep-expr "last" --set ana.activated "['base','expRelation']"
int main(void) {
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    return 0;
}

// Initializing an array with pointers
void example1(void) {
    int top;

    int a[42];
    int *ptr = &a;

    *ptr = 42;
    ptr++;

    assert(a[0] == 42);
    assert(a[1] == 42); // UNKNOWN

    *ptr = 42;
    assert(a[0] == 42);
    assert(a[1] == 42);
    ptr++;

    *ptr = 42;
    ptr++;
    *ptr = 42;
    ptr++;
    *ptr = 42;
    ptr++;
    *ptr = 42;
    ptr++;


    int i = 5;
    assert(a[i] == 42);

    if(top) {
        i++;
    }

    assert(a[i] == 42); // UNKNOWN
}

// Tests correct handling when pointers may point to several different things
void example2() {
  int array1[10000000];
  int array2[10000000];

  int* ptr;

  if(rand()) {
    ptr = &array1;
    *ptr = 5;

    assert(*ptr == 5);
  }
  else {
    ptr = &array2;
    *ptr = 5;

    assert(*ptr == 5);
  }

  // Since ptr could point to different arrays, the update here can not be precise
  *ptr = 6;

  assert(*ptr == 6); // UNKNOWN
}

void example3(void) {
  int array1[5];
  int *ptr = &array1;

  for(int i =0; i <5; i++) {
    *ptr = 42;
    assert(*ptr == 42);
    ptr++;
  }
}

void example4(void) {
  int array1[5];
  int *ptr = &array1;
  int *end = &(array1[5]);

  while(ptr <= end) {
    *ptr = 42;
    assert(*ptr == 42);
    ptr++;
  }

  // In an ideal world, I would like to have information about array1[0] and so on. For this the <= would need yo improve
}

void example5(void) {
  int array1[5];
  int *ptr = &(array1[4]);

  *ptr = 42;
  ptr--;
  *ptr = 42;
  ptr--;
  *ptr = 40;

  assert(*ptr == 40);
  assert(array1[4] == 42);
  assert(array1[3] == 42);
  assert(array1[2] == 40);
  assert(array1[0] == 42); // UNKNOWN
}

void example6(void) {
  int array1[100];
  int* ptr = &array1;

  *ptr = 5;
  int v = *ptr;
  assert(v == 5);

  ptr++;
  *ptr = 6;
  ptr++;
  *ptr = 7;

  // This is necessary for the tests that we are doing later
  int k = ptr-&array1;
  assert(k == 2);
  int m = ptr-array1;
  assert(m == 2);

  int* np = &array1;
  np++;
  np++;
  int x = *np;
  assert(x==7);
}

void example7(void) {
  int top;

  int arr1[42];
  int arr2[42];
  int *ptr;

  for(int i = 0; i < 42; i++) {
    arr1[i] = 4;
    arr2[i] = 4;
  }

  ptr = &arr1[7];

  if(top) {
    ptr = &arr2[7];
  }

  *ptr = 9;

  // Case ptr = &arr1[7]
  //    arr1 -> (ptr-arr1, ([4,4], [9,9],[4,4]))
  //    arr2 -> (-,[4,4])

  // Case ptr = &arr2[7]
  //    arr1 -> (-, [4,4])
  //    arr2 -> (ptr-arr2, ([4,4], [9,9],[4,4]))

  // LUB:
  //    arr1 -> (-, [4,9])
  //    arr2 -> (-, [4,9])
  int x = arr1[7];
  assert(x == 3); // FAIL
  assert(x == 4); // UNKNOWN
  assert(x == 9); // UNKNOWN
  assert(x == 10); // FAIL
}
