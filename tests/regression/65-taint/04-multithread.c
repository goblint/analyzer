// NOMARSHAL PARAM: --set ana.activated[+] taintPartialContexts
#include <goblint.h>
#include <pthread.h>

int glob_rel;
int glob_keep;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void f(pthread_mutex_t *mutex_ptr) {
  pthread_mutex_unlock(mutex_ptr);
}

void g() {
}

void *t_fun(void *arg) {
  int x_t_fun = -2;
  int context = 1;

  // f releases the mutex, so information should be lost after call, even though glob_rel is untainted
  pthread_mutex_lock(&mutex1);
  glob_rel = 1;
  f(&mutex1);
  __goblint_check(glob_rel == 1); //UNKNOWN!

  // g does nothing, so glob_keep information can be kept
  pthread_mutex_lock(&mutex2);
  glob_keep = 1;
  g();
  __goblint_check(glob_keep == 1);
  pthread_mutex_unlock(&mutex2);

  //sanity
  __goblint_check(x_t_fun == -2);
  return NULL;
}

int main() {
  int x_main = -1;
  int x_arg = -3;
  pthread_t id;

  pthread_create(&id, NULL, t_fun, &x_arg);
  pthread_join (id, NULL);

  //sanity
  __goblint_check(x_main == -1);
  __goblint_check(x_arg == -3);
}
