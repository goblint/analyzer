// PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g = 42; // matches write in t_fun
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 42;
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int r, t;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  if (r) {
    g = 17;
    pthread_mutex_unlock(&B); // publish to g#prot
    pthread_mutex_lock(&B);
  }
  // locally written g is only in one branch, g == g#prot should be forgotten!
  t = g;
  assert(t == 17); // UNKNOWN!
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return 0;
}
