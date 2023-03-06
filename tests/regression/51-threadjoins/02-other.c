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
  h++; // RACE!
  pthread_t id2;
  pthread_create(&id2, NULL, t_fun, NULL);
  pthread_join(id2, NULL);
  // should be empty
  return NULL;
}

int main(void) {
  int t;

  pthread_t id2[10];
  for(int i =0; i < 10;i++) {
  pthread_create(&id2[i], NULL, t_benign, NULL);
  }

  pthread_join(id2[2], NULL);

  // should be empty

  g++; // RACE!
  h++; // RACE!

  return 0;
}
