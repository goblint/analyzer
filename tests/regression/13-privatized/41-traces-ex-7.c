#include <pthread.h>
#include <assert.h>

int g = 0; // matches unsound read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t D = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&D);
  pthread_mutex_lock(&A);
  g = 17;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&D);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&D);
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&D);
  assert(g == 0); // UNKNOWN!
  return 0;
}
