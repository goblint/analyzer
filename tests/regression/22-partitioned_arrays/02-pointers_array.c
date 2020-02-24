// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void) {
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();
    example10();
    example11();
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

  // In an ideal world, I would like to have information about array1[0] and so on. For this the <= would need to improve, so that ptr is known to point to {array1[5,5]}
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

void example8(void) {
  int a[42][42];

  for(int i = 0; i < 42; i++) {
    for(int j=0;j < 42; j++) {
      a[i][j] = 0;
    }
  }

  a[14][0] = 3;

  int* ptr = a[7];
  int x = *(ptr+7);
  assert(x == 3); //FAIL

  int (*ptr2)[42];
  ptr2 = a+7;
  x = (*ptr2)[6];
  assert(x == 3);  //FAIL
  printf("x is %d\n", x);
}

struct a {
  int x[42];
  int y;
};

void example9() {
  int a[42][42];
  int (*ptr2)[42];
  int *y;
  int i, j, x;

  for(i = 0; i < 42; i++) {
    for(j=0;j < 42; j++) {
      a[i][j] = 0;
    }
  }

  a[14][0] = 3;
  ptr2 = a+7;
  y = (ptr2+1)[6];
  assert(*y == 3);
}

int example10() {
  struct a x[42];
  int i, j, y, *ptr;

  for(i = 0; i < 42; i++) {
    for(j=0;j < 42; j++) {
      x[i].x[j] = 0;
    }
  }
  x[3].x[3] = 7;

  ptr = x[3].x;
  y = *(ptr + 3);
  assert(y == 0); //FAIL
  printf("y is %d", y);
}

void foo(int (*a)[40]){
  int x = (*(a + 29))[7];
  assert(x == 23); //UNKNOWN
}

void example11()
{
  int b[40][40];
  b[7][7] = 23;

  foo(b);
}
