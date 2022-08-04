// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval
extern int __VERIFIER_nondet_int();

#include <assert.h>

void foo(int a, int *pb) {
  int b = *pb;
   // base knows a == 4 and b == 4, apron only knows a == 4
  __goblint_check(a == b);
}

void main() {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  int z = __VERIFIER_nondet_int(); //rand
  int a = __VERIFIER_nondet_int(); //rand
  int b = __VERIFIER_nondet_int(); //rand
  if (x < y && y < z) {
    // base doesn't know anything, apron knows x < y < z
    __goblint_check(x < y);
    __goblint_check(y < z);
    __goblint_check(x < z);

    if (3 <= x && z <= 5) {
      // base knows 3 <= x and z <= 5, apron knows x == 3 and y == 4 and z == 5
      a = y; // base should now know a == 4 via EvalInt query

      __goblint_check(x == 3);
      __goblint_check(y == 4);
      __goblint_check(z == 5);
      __goblint_check(a == 4);

      b = 4;
      foo(a, &b); // base should add a == 4 and b == 4 to context, apron only adds a == 4
    }
  }
}
