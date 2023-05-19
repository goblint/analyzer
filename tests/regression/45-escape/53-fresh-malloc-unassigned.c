// PARAM: --set ana.activated[+] mallocFresh --set ana.activated[-] mhp --set ana.thread.domain plain
#include <pthread.h>
#include <stdlib.h>

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun2(void *arg) {
  int *i = arg;
  pthread_mutex_lock(&A);
  *i = 10; // NORACE
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  int r, *i;
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded

  if (r) {
    i = malloc(sizeof(int));
  } else {
    i = NULL;
  }

  i = i == NULL ? malloc(sizeof(int)) : i;
  *i = 5; // NORACE (fresh)
  pthread_create(&id2, NULL, t_fun2, i);
  return 0;
}