// PARAM: --set ana.activated[+] deadlock --set ana.activated[+] threadJoins
#include <pthread.h>

pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *thread() {
  pthread_mutex_lock(&m2); //DEADLOCK
  pthread_mutex_lock(&m1); //DEADLOCK
  pthread_mutex_unlock(&m2);
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

  pthread_mutex_lock(&m1); // DEADLOCK
  pthread_create(&tid2, NULL, thread, NULL);
  pthread_mutex_lock(&m2); //DEADLOCK

  return 0;
}
