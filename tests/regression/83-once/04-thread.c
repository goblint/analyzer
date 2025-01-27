// PARAM: --set pre.cppflags[+] "-DGOBLINT_NO_PTHREAD_ONCE" --set ana.activated[+] pthreadOnce
#include <pthread.h>
#include <stdio.h>

int g;
int h;
int i;
pthread_once_t once = PTHREAD_ONCE_INIT;
pthread_once_t i_once = PTHREAD_ONCE_INIT;
pthread_mutex_t mut;

void *t_other(void* arg) {
  g = 17; //RACE

  pthread_mutex_lock(&mut);
  i = 7; //NORACE
  pthread_mutex_unlock(&mut);
}

void nesting() {
  h = 5; //NORACE
}

void fun() {
  // Even though this is only called inside the once, the accesses in the new thread and the accesses here can happen in parallel
  // Checks that active is not passed to the created thread, but seen is passed
  pthread_t tid = pthread_create(&tid, NULL, t_other, NULL);
  g = 42; //RACE

  h = 8; //NORACE
  // Active onces get passed to the callee and back to the caller
  nesting();
  h = 12; //NORACE
}

void ifun() {
  i = 11; //NORACE
}

void *t_fun(void *arg) {
  pthread_once(&i_once, ifun);
  pthread_once(&once, fun);
  return NULL;
}

int main(void) {
  pthread_t id;
  int top;

  pthread_create(&id, NULL, t_fun, NULL);

  pthread_once(&i_once, ifun);
  pthread_once(&once, fun);

  h = 5; //NORACE

  return 0;
}
