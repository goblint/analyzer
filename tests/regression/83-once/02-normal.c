// PARAM: --set pre.cppflags[+] "-DGOBLINT_NO_PTHREAD_ONCE" --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void fun() {
  g = 42; //NORACE
}


void *t_fun(void *arg) {
  pthread_once(&once, fun);

  pthread_mutex_lock(&mutex1);
  g = 10; //NORACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_once(&once, fun);

  pthread_mutex_lock(&mutex1);
  g = 11; //NORACE
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
