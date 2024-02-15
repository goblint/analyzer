// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid-atomic --set sem.int.signed_overflow assume_none --enable ana.apron.threshold_widening --set ana.path_sens[+] threadflag --enable ana.apron.strengthening
// TODO: -atomic unneeded?
#include <pthread.h>
#include <assert.h>
#include <goblint.h>

extern int  __VERIFIER_nondet_int();
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x = 0;
int g = 0;

void* inc()
{
  int n = __VERIFIER_nondet_int();

  while (x < n) {
    __VERIFIER_atomic_begin();
    x++;
    __VERIFIER_atomic_end();

    __VERIFIER_atomic_begin();
    assert(x >= g);
    __VERIFIER_atomic_end();
  }

  return 0;
}

int main()
{
  pthread_t tid;
  pthread_create(&tid, 0, inc, 0);

  int val = __VERIFIER_nondet_int();
  __VERIFIER_atomic_begin();
  __goblint_globalize(&val);
  g = val; x = val;
  __VERIFIER_atomic_end();

  __VERIFIER_atomic_begin(); // TODO: atomic needed?
  assert(x >= val);
  __VERIFIER_atomic_end();
  return 0;
}
