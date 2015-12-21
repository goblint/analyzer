// PARAM: --sets ana.activated[+] deadlock
#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&mutex2); // NODEADLOCK
  g1 = g2 + 1;
  pthread_mutex_unlock(&mutex2);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

void *t2(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&mutex2); // NODEADLOCK
  g2 = g1 + 1;
  pthread_mutex_unlock(&mutex2);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
  int i;
  for (i = 0; i < 10000; i++) {
    pthread_create(&id1, NULL, t1, NULL);
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join (id1, NULL);
    pthread_join (id2, NULL);
    printf("%d: g1 = %d, g2 = %d.\n", i, g1, g2);
  }
  return 0;
}
