// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.base.privatization none
#include <assert.h>

struct kala {
  int i;
  int a[5];
};

struct kalaw {
  int* a;
};

struct kass {
  int v;
};

union uArray {
  int a[5];
  int b[5];
};

union uStruct {
  int b;
  struct kala k;
};

int main(void) {
  example1();
  example2();
  example3();
  example4();
  example5();
  example6();
  example7();
  example8();
  return 0;
}

void example1() {
  struct kala l;
  int i = 0;
  int top;

  while (i < 5) {
      l.a[i] = 42;
      i++;

      // Check assertion that should only hold later does not already hold here
      __goblint_check(l.a[4] == 42); //UNKNOWN
  }

  // Check the array is correctly initialized
  __goblint_check(l.a[1] == 42);
  __goblint_check(l.a[2] == 42);
  __goblint_check(l.a[3] == 42);
  __goblint_check(l.a[4] == 42);

  // Destructively assign to i
  i = top;

  // Check the array is still known to be completly initialized
  __goblint_check(l.a[1] == 42);
  __goblint_check(l.a[2] == 42);
  __goblint_check(l.a[3] == 42);
  __goblint_check(l.a[4] == 42);
}

void example2() {
  struct kala kalas[5];

  int i2 = 0;

  while (i2 < 4) {
      int j2 = 0;
      while (j2 < 5) {
          kalas[i2].a[j2] = 8;
          j2++;
      }
      i2++;
  }

  // Initialization has not proceeded this far
  __goblint_check(kalas[4].a[0] == 8); //UNKNOWN
  __goblint_check(kalas[0].a[0] == 8);
}

void example3() {
  struct kala xnn;
  for(int l=0; l < 5; l++) {
      xnn.a[l] = 42;
  }

  __goblint_check(xnn.a[3] == 42);
}

void example4() {
  struct kala xn;

  struct kala xs[5];

  for(int j=0; j < 4; j++) {
    xs[j] = xn;
    for(int k=0; k < 5; k++) {
      xs[j].a[k] = 7;
    }
  }

  __goblint_check(xs[3].a[0] == 7);
}

void example5() {
  // This example is a bit contrived to show that splitting and moving works for
  // unions
  union uArray ua;
  int i3 = 0;
  int top;
  int *i = &top;

  ua.a[*i] = 1;

  while (i3 < 5) {
    ua.a[i3] = 42;
    i3++;
  }

  __goblint_check(ua.a[i3 - 1] == 42);

  ua.b[0] = 3;
  __goblint_check(ua.b[0] == 3);

  // -------------------------------
  union uStruct us;
  int i4 = 0;

  us.b = 4;
  us.k.a[i4] = 0;
  __goblint_check(us.b == 4); // UNKNOWN
  __goblint_check(us.k.a[0] == 0);
  __goblint_check(us.k.a[3] == 0); // UNKNOWN

  while (i4 < 5) {
      us.k.a[i4] = 42;
      i4++;
  }

  __goblint_check(us.k.a[1] == 42);
  __goblint_check(us.k.a[0] == 0); // FAIL
}

void example6() {
  int a[42];
  int i = 0;

  struct kass k;
  k.v = 7;

  while(i < 42) {
      a[i] = 0;
      i++;
  }

  i = 0;

  a[k.v] = 2;
  k.v = k.v+1;

  __goblint_check(a[k.v] != 3);
}

void example7() {
  // Has no asserts, just checks this doesn't cause an infinite loop
  int a[42];
  int i = 0;

  while(i < 40) {
      a[i] = 0;
      i++;
  }

  a[a[0]] = 2;
}

// Test correct behavior with more involved expression in subscript operator
void example8() {
  int a[42];
  union uArray ua;

  ua.a[0] = 0;
  ua.a[1] = 0;
  ua.a[2] = 0;
  ua.a[3] = 0;
  ua.a[4] = 0;

  int i = 0;
  int *ip = &i;

  a[ua.a[*ip]] = 42;
  ip++;
  __goblint_check(a[ua.a[*ip]] == 42); //UNKNOWN
}
