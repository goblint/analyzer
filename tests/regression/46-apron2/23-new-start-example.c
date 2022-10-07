// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set ana.apron.privatization mutex-meet-tid-cluster12
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
int i = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_one(void *arg) {
  int x;
  pthread_mutex_lock(&A);
  x = h;
  i = x;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_two(void *arg) {
  int x,y;
  pthread_mutex_lock(&A);
  g = x;
  h = y;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;
  int a,x,y,z;

  // Force multi-threaded handling
  pthread_t id1;
  pthread_create(&id1, NULL, t_one, NULL);

  pthread_t id2;
  pthread_create(&id2, NULL, t_two, NULL);

  pthread_mutex_lock(&A);
  g = x; h = y; i = z;
  pthread_mutex_unlock(&A);

  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  g = a; h = a; i = a;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  __goblint_check(g == h);
  __goblint_check(h == i);
  pthread_mutex_unlock(&A);

  return 0;
}
