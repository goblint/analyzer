// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --set ana.specification 'CHECK( init(main()), LTL(G ! call(reach_error())) )' --enable ana.int.interval
// previously both branches dead
// simplified from 28-race_reach/06-cond_racing1
#include <pthread.h>
#include <assert.h>

int __VERIFIER_nondet_int();

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int main() {
  int i = __VERIFIER_nondet_int();

  if (i) pthread_mutex_lock(&mutex);

  if (i)
    assert(1); // reachable
  else
    assert(1); // reachable

  assert(1); // reachable

  return 0;
}