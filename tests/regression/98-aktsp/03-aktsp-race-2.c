// PARAM: --set ana.activated[-] mhp --set ana.activated[-] threadid --enable allglobs
#include <pthread.h>

int g = 0;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *arg) {
  pthread_mutex_lock(&m);
  g++;
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t t1;
  pthread_create(&t1, NULL, &foo, NULL);

  pthread_mutex_lock(&m);
  g++;
  pthread_mutex_unlock(&m);
  return 0;
}
