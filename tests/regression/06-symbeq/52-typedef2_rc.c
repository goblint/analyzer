// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
// MANUAL must have race on (int), not safe on (int) and (int2)
#include<pthread.h>

typedef int int2;

extern int *get_s();

void *t_fun(void *arg) {
  int2 *s = get_s();
  *s = 5; // RACE!
  return NULL;
}

int main () {
  int *d;
  pthread_t id;
  pthread_mutex_t *m;

  d = get_s();

  pthread_create(&id,NULL,t_fun,NULL);
  *d = 8; // RACE!

  return 0;
}
