// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.relation.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  g = 7;
  return NULL;
}

int main(void) {
  int t;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join(id,NULL);

  g = h;
  __goblint_check(g == h);

  // __goblint_assume_join for something Goblint knows is joined should not worsen precision
  __goblint_assume_join(id);

  __goblint_check(g == h);

  return 0;
}
