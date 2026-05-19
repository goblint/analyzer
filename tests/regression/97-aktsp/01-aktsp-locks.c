// PARAM: --set ana.activated[-] mhp --set ana.activated[-] threadid --set ana.activated[+] maylocks --set ana.activated[+] pthreadMutexType
#include <pthread.h>

int g = 0;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *arg) {
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m1);
  g++;
  pthread_mutex_unlock(&m1);
  return NULL;
}

void *bar(void *arg) {
  pthread_mutex_lock(&m2);
  g++;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m2);
  return NULL;
}

int main() {
  pthread_t t1, t2;
  pthread_create(&t1, NULL, &foo, NULL);
  pthread_create(&t2, NULL, &bar, NULL);

  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  g++;
  pthread_mutex_unlock(&m1);
  return 0;
}
