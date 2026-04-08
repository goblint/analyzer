// PARAM: --enable ana.sv-comp.functions
/*-----------------------------------------------------------------------------
 * mutex.c - Concurrent program using locking to access a shared variable
 *-----------------------------------------------------------------------------
 * Author: Frank Schüssele
 *   Date: 2023-07-11
 *---------------------------------------------------------------------------*/
#include <pthread.h>
#include <goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int used;
pthread_mutex_t m;

void* producer()
{
  while (1) {
    pthread_mutex_lock(&m);
    used++;
    used--;
    pthread_mutex_unlock(&m);
  }

  return 0;
}

int main()
{
  pthread_t tid;

  pthread_mutex_init(&m, 0);
  pthread_create(&tid, 0, producer, 0);

  pthread_mutex_lock(&m);
  __goblint_check(used == 0);
  pthread_mutex_unlock(&m);

  pthread_mutex_destroy(&m);
  return 0;
}
