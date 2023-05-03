// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadCreateWrapper --set ana.thread.wrappers[+] my_pthread_create --set ana.thread.unique_thread_id_count 2
#include <pthread.h>
#include <stdio.h>

// mark this as a wrapper, which is called multiple times in the same place
int my_pthread_create(
  pthread_t *restrict thread,
  const pthread_attr_t *restrict attr,
  void *(*start_routine)(void *),
  void *restrict arg
) {
  return pthread_create(thread, attr, start_routine, arg);
}

// this is not marked as a wrapper; instead each call to my_pthread_create is given a unique ID
int my_other_pthread_create(
  pthread_t *restrict thread,
  const pthread_attr_t *restrict attr,
  void *(*start_routine)(void *),
  void *restrict arg
) {
  return my_pthread_create(thread, attr, start_routine, arg);
}

// uncomment to remove the wrapper
// #define my_other_pthread_create pthread_create

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main() {
  pthread_t id1;
  my_other_pthread_create(&id1, NULL, t_fun, NULL);
  pthread_t id2;
  my_other_pthread_create(&id2, NULL, t_fun, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);

  g = 2; // NORACE

  return 0;
}
