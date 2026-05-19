// PARAM: --set ana.activated[-] mhp --set ana.activated[-] threadid
#include <pthread.h>

int g = 0;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *arg) {
  pthread_mutex_lock(&m2);
  g++;
  pthread_mutex_unlock(&m2);
  return NULL;
}

int main() {
  pthread_t t1;
  pthread_create(&t1, NULL, &foo, NULL);

  pthread_mutex_lock(&m1);
  g++;
  pthread_mutex_unlock(&m1);
  return 0;
}
