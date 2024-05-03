//PARAM: --set ana.activated[+] apron --enable ana.int.congruence --enable ana.sv-comp.functions
#include <pthread.h>
#include <stdlib.h>
#include <goblint.h>

pthread_mutex_t mutex;

void *fun(void* args)
{
   pthread_mutex_lock(&mutex);
   pthread_mutex_unlock(&mutex);

   __goblint_assert(1); //Reachable

   __VERIFIER_atomic_begin();
   __goblint_assert(1); //Reachable
   __VERIFIER_atomic_end();

  __goblint_assert(1); //Reachable
}

int main(void)
{
 pthread_t t;
 pthread_create(&t, ((void *)0), fun, ((void *)0));
}
