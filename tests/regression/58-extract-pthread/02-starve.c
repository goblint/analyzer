// PARAM: --set ana.activated[+] extract-pthread
#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex1);
  return NULL;
}


int main(void) {
  pthread_t id1, id2;
  int i;

  pthread_mutex_lock(&mutex1);
  pthread_create(&id1, NULL, t1, NULL);
  printf("%d: g1 = %d, g2 = %d.\n", i, g1, g2);

  return 0;
}
