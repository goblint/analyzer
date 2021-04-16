#include <pthread.h>
#include <assert.h>

int g;

void *t_fun(void *arg) {
  return NULL;
}

void main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  g = 1;
  assert(g); // TODO (Mine's analysis would succeed, our mine-W doesn't)
}
