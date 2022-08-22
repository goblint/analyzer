#include<stdio.h>
#include<assert.h>

void proc(int *x, int *y) {
  *x = *y + 3;
}

void evil (int *x, int *y, int *z) {
  (*x)++;
  *z = *y;
}

int main () {
  int z = 1;
  int k;
  int *x, *y;
  int a,b,c;

  // simple aliasing
  x = &z; y = &z;
  (*x)++;
  __goblint_check(*y == 2);

  // may-aliasing
  if (k) x = &k;
  (*x)++;
  __goblint_check(*y == 2); // UNKNOWN

  // Function call return test
  z = 7;
  proc(&z, &z);
  __goblint_check(z == 10);
  printf("%d\n",z);


  // Aliasing tests of fun args

  a = 3; b = 5; c = 7;
  evil(&a, &b, &c);
  printf("%d %d %d\n",a,b,c);
  __goblint_check(a == 4);
  __goblint_check(b == 5);
  __goblint_check(c == 5);

  a = 3; b = 5;
  evil(&a, &a, &a);
  printf("%d %d\n",a,b);
  __goblint_check(a == 4);
  __goblint_check(b == 5);

  a = 3; b = 5;
  evil(&a, &b, &a);
  printf("%d %d\n",a,b);
  __goblint_check(a == 5);
  __goblint_check(b == 5);

  a = 3; b = 5;
  evil(&a, &a, &b);
  printf("%d %d\n",a,b);
  __goblint_check(a == 4);
  __goblint_check(b == 4);

  // may aliasing

  a = 3; b = 3; c = 7;
  x = &a; if (k) x = &b;
  evil(x, x, &c);
  printf("%d %d %d\n",a,b,c);
  __goblint_check(a == 3); // UNKNOWN
  __goblint_check(b == 3); // UNKNOWN
  __goblint_check(c == 4); // UNKNOWN

  a = 3; b = 5; c = 7;
  x = &a; if (k) x = &b;
  evil(x, &a, &c);
  printf("%d %d %d\n",a,b,c);
  __goblint_check(a == 3); // UNKNOWN
  __goblint_check(b == 5); // UNKNOWN
  __goblint_check(c == 6); // UNKNOWN

  return 0;
}
