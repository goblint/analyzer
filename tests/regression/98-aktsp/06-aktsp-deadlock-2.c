// PARAM: --set ana.activated[-] mhp --set ana.activated[-] threadid --set ana.activated[+] deadlock
#include <pthread.h>

int g1 = 10;
int g2 = 10;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *arg) {
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  g2--;
  g1++;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return NULL;
}

int main() {
  pthread_t t1;
  pthread_create(&t1, NULL, &foo, NULL);

  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  g1--;
  g2++;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return 0;
}
