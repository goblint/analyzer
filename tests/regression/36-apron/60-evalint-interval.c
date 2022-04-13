// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval
extern int __VERIFIER_nondet_int();

#include <assert.h>

void foo(int *pb) {
  int b = *pb;
   // base knows 4 <= a <= 5 and pb == &a, apron only knows nothing
  assert(4 <= b);
  assert(b <= 5);
}

void main() {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  int z = __VERIFIER_nondet_int(); //rand
  int a = __VERIFIER_nondet_int(); //rand
  int b = __VERIFIER_nondet_int(); //rand
  if (x < y && y < z) {
    // base doesn't know anything, apron knows x < y < z
    assert(x < y);
    assert(y < z);
    assert(x < z);

    if (3 <= x && z <= 6) {
      // base only knows 3 <= x and z <= 6
      a = y; // base should now know 4 <= a <= 5 via EvalInt query

      assert(x <= 4);
      assert(4 <= y);
      assert(y <= 5);
      assert(5 <= z);

      foo(&a); // base should add 4 <= a <= 5 and pb == &a to context, apron only adds nothing
    }
  }
}
