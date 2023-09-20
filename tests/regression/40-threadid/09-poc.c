// PARAM: --set ana.base.arrays.domain partitioned --enable ana.int.interval
#include <goblint.h>
#include <pthread.h>


void *t_fun(void *arg) {
  return 5;
}

int main() {
  pthread_t idtmp;
  pthread_t id[10];
  int x;

  pthread_create(&id[0], NULL, t_fun, NULL);
  x = 8;

  for(int i =0; i < 10; i++) {
    pthread_create(&idtmp, NULL, t_fun, NULL);
    id[i] = idtmp;
  }

  x = 2;

  for(int i =0; i < 10; i++) {
    // Pulled into temporary assignment, as set in pthread_join does not provide needed information to
    // partitioned arrays
    idtmp = id[i];
    pthread_join(idtmp, &x); //Providing a pointer to x to go into the code that handles it
    id[i] = idtmp;
  }

  // We know all threads who have their tids in the array id have been joined
  // Only additional thing that is needed is proof that the array contains all thread ids that have been created
  // e.g. by all distinct
}
