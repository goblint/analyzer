// PARAM: --set ana.activated[+] deadlock
#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t *p, *q;

void *t1(void *arg) {
  pthread_mutex_lock(p);
  pthread_mutex_lock(q); // DEADLOCK
  g1 = g2 + 1;
  pthread_mutex_unlock(q);
  pthread_mutex_unlock(p);
  return NULL;
}

void *t2(void *arg) {
  pthread_mutex_lock(p);
  pthread_mutex_unlock(p);
  pthread_mutex_lock(q); // DEADLOCK
  g2 = g1 + 1;
  pthread_mutex_unlock(q);
  pthread_mutex_unlock(p);
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
  int i;
  pthread_mutex_t *a;

  for (i=0; i < 10; i++){
    a = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(a,0);
    if (i==3)
      p = a;
    if (i==7)
      q = a;
  }

  for (i = 0; i < 1000000; i++) {
    pthread_create(&id1, NULL, t1, NULL);
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join (id1, NULL);
    pthread_join (id2, NULL);
    printf("%d: g1 = %d, g2 = %d.\n", i, g1, g2);
  }
  return 0;
}
