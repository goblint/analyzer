// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-tid --set ana.path_sens[+] threadflag --set sem.int.signed_overflow assume_none --enable ana.apron.strengthening
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
  int g2 = g;
  int used2 = used;
  __goblint_check(used2 == 0); // UNKNOWN! (currently unsound)
  __goblint_check(g2 == 1 || used2 == 0);
  __VERIFIER_atomic_end();

  __VERIFIER_atomic_begin();
  g = 1;
  pthread_mutex_lock(&m);
  __VERIFIER_atomic_end();

  __goblint_check(used == 0);

  __VERIFIER_atomic_begin();
  g = 0;
  pthread_mutex_unlock(&m);
  __VERIFIER_atomic_end();

  pthread_mutex_destroy(&m);
  return 0;
}
