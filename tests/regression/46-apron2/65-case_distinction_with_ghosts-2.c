// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-atomic --set sem.int.signed_overflow assume_none
#include <pthread.h>
#include <assert.h>

extern int  __VERIFIER_nondet_int();
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x = 0;
int g = 0;

void* inc()
{
  __VERIFIER_atomic_begin();
  assert(g != 1 || x >= 42);
  __VERIFIER_atomic_end();
  return 0;
}

int main()
{
  pthread_t tid;
  pthread_create(&tid, 0, inc, 0);
  __VERIFIER_atomic_begin();
  g = 1; x = 42;
  __VERIFIER_atomic_end();

  assert(x >= 42); // TODO
  return 0;
}
