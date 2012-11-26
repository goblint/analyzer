// PARAM: --set custom_libc true
#include<pthread.h>
#include<ftw.h>

int glob;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex2);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int reset_glob(char const *path, struct stat const *sp, int f) {
  pthread_mutex_lock(&mutex1);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  return 0;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  ftw(".", reset_glob, 10);
  pthread_join (id, NULL);
  return 0;
}
