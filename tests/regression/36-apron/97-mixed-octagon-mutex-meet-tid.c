// SKIP PARAM: --set solver td3 --enable ana.int.interval  --enable exp.partition-arrays.enabled  --set ana.activated "['base','mutex','threadid','threadflag','expRelation','mallocWrapper','apron']" --set ana.path_sens[+] threadflag --set exp.privatization mutex-meet --set exp.apron.privatization mutex-meet-tid --set ana.apron.domain "octagon" --enable exp.apron.priv.only-interval
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf, adapted

#include <pthread.h>

int g = 0;
int h = 0;
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *set_globals(void *arg) {
  int x = 2;
  pthread_mutex_lock(&mtx);
  h = x;
  g = x;
  pthread_mutex_unlock(&mtx);
  return NULL;
}

int main() {
  pthread_t thread;
  pthread_create(&thread, NULL, &set_globals, NULL);

  pthread_mutex_lock(&mtx);
  // This should be unknown if we only track interval information about globals
  assert(h == g); // UNKNOWN
  pthread_mutex_unlock(&mtx);

  return 0;
}
