// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-atomic --set sem.int.signed_overflow assume_none
/*-----------------------------------------------------------------------------
 * mutex_with_ghosts.c - Annotated concurrent program with ghost variables for
 *                       witness validation using locking to access a shared
 *                       variable
 *-----------------------------------------------------------------------------
 * Author: Frank Schüssele
 *   Date: 2023-07-11
 *---------------------------------------------------------------------------*/
#include <pthread.h>
#include <goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int used;
int g = 0;
pthread_mutex_t m;

void* producer()
{
  while (1) {
    __VERIFIER_atomic_begin();
    g = 1;
    pthread_mutex_lock(&m);
    __VERIFIER_atomic_end();

    used++;
    used--;

    __VERIFIER_atomic_begin();
    g = 0;
    pthread_mutex_unlock(&m);
    __VERIFIER_atomic_end();
  }

  return 0;
}

int main()
{
  pthread_t tid;

  pthread_mutex_init(&m, 0);
  pthread_create(&tid, 0, producer, 0);

  __VERIFIER_atomic_begin();
  __goblint_check(g == 1 || used == 0); // TODO (unprotected invariant precision loss?)
  __VERIFIER_atomic_end();

  __VERIFIER_atomic_begin();
  g = 1;
  pthread_mutex_lock(&m);
  __VERIFIER_atomic_end();

  __goblint_check(used == 0); // TODO (read/refine? of used above makes used write-unprotected)

  __VERIFIER_atomic_begin();
  g = 0;
  pthread_mutex_unlock(&m);
  __VERIFIER_atomic_end();

  pthread_mutex_destroy(&m);
  return 0;
}
