// PARAM: --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
pthread_once_t once1 = PTHREAD_ONCE_INIT;
 // PTHREAD_ONCE_INIT is `0`, so coincides with the default value for global variables (c.f. also PTHREAD_MUTEX_INITIALIZER)
pthread_once_t once2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

pthread_once_t *ptr;

void fun() {
  g = 42; //RACE
}


void *t_fun(void *arg) {
  pthread_once(ptr, fun);

  pthread_mutex_lock(&mutex1);
  g = 10; //RACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  int top;
  pthread_create(&id, NULL, t_fun, NULL);

  ptr = &once1;

  if(top) {
    ptr = &once2;
  }

  pthread_once(ptr, fun);

  pthread_mutex_lock(&mutex1);
  g = 11; //RACE
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
