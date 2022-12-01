// PARAM: --disable sem.unknown_function.spawn --disable sem.unknown_function.invalidate.globals
#include <assert.h>

struct S1 {
  void (*f1)(void);
};

struct S2 {
  void (*f2)(void);
};

void foo() {
  assert(1); // reachable
}

void bar() {
  assert(1); // reachable
}

int main() {
  struct S1 s1 = {.f1 = &foo};
  struct S2 s2 = {.f2 = &bar};
  int r; // rand
  void *sp;

  if (r)
    sp = &s1;
  else
    sp = &s2;
  // simulate imprecision
  // in chrony this wouldn't be path-based but joined in the global invariant

  // one of these field accesses is randomly invalid
  void (*fp1)(void) = ((struct S1*)sp)->f1;
  void (*fp2)(void) = ((struct S2*)sp)->f2;
  // but we shouldn't forget &foo and &bar here and consider both dead
  fp1();
  fp2();
  return 0;
}
