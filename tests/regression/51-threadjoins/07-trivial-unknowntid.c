//PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

void *t_benign(void *arg) {
  h++; // NORACE
  pthread_t id2;
  pthread_create(&id2, NULL, t_fun, NULL);
  foo(&id2);
  pthread_join(id2, NULL);
  return NULL;
}

int main(void) {
  int t;

  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);
  pthread_join(id2, NULL);
  // t_benign and t_fun should be in here

  g++; // RACE!
  h++; // NORACE

  return 0;
}
