#include<pthread.h>
#include<ftw.h>
#include "racemacros.h"

int glob;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;


int reset_glob(char const *path, struct stat const *sp, int f) {
  pthread_mutex_lock(&mutex1);
  access(glob);
  pthread_mutex_unlock(&mutex1);
  return 0;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex2);
  assert_racefree(glob); // UNKNOWN
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int main() {
  create_threads(t);
  ftw(".", reset_glob, 10);
  join_threads(t);
  return 0;
}
