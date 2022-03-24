// PARAM: --set ana.activated[+] deadlock --set ana.path_sens[-] mutex
#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;

void *t1(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&mutex2); // DEADLOCK
  g1 = g2 + 1;
  pthread_mutex_unlock(&mutex2);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

void *t2(void *arg) {
  int k = rand() % 2;
  int l = rand() % 2;
  pthread_mutex_t *m, *n;
  if (k)
    m = &mutex2;
  else
    m = &mutex3;
  if (l)
    n = &mutex2;
  else
    n = &mutex3;
  pthread_mutex_lock(m);
  pthread_mutex_unlock(n);
  pthread_mutex_lock(&mutex1); // DEADLOCK
  g2 = g1 + 1;
  pthread_mutex_unlock(&mutex1);
  pthread_mutex_unlock(m);
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
  int i;
  for (i = 0; i < 1000000; i++) {
    pthread_create(&id1, NULL, t1, NULL);
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join (id1, NULL);
    pthread_join (id2, NULL);
    printf("%d: g1 = %d, g2 = %d.\n", i, g1, g2);
  }
  return 0;
}
