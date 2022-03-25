// SKIP PARAM: --set ana.activated[+] deadlock --set ana.activated[+] threadJoins
// TODO: https://github.com/goblint/analyzer/issues/667
#include <pthread.h>

int x;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m5 = PTHREAD_MUTEX_INITIALIZER;

void *thread() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock(&m1);
  // We forgot to unlock
  // pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);

  return NULL;
}

void *noOpThread() {
  return NULL;
}

int main() {
  pthread_t tid1;
  pthread_t tid2;

  pthread_create(&tid1, NULL, noOpThread, NULL);

  pthread_create(&tid2, NULL, thread, NULL);
  pthread_join(tid2,NULL);
  pthread_mutex_lock(&m2); // TODO DEADLOCK

  return 0;
}
