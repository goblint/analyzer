// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval
extern int __VERIFIER_nondet_int();

#include <goblint.h>

void foo(int *pb) {
  int b = *pb;
   // base knows 4 <= a <= 5 and pb == &a, apron only knows nothing
  __goblint_check(4 <= b);
  __goblint_check(b <= 5);
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

    if (3 <= x && z <= 6) {
      // base only knows 3 <= x and z <= 6
      a = y; // base should now know 4 <= a <= 5 via EvalInt query

      __goblint_check(x <= 4);
      __goblint_check(4 <= y);
      __goblint_check(y <= 5);
      __goblint_check(5 <= z);

      foo(&a); // base should add 4 <= a <= 5 and pb == &a to context, apron only adds nothing
    }
  }
}
