#include <pthread.h>
#include <assert.h>

int g;

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  g = 1;
  assert(g); // TODO (Mine's analysis would succeed, our mine-W doesn't)
  return 0;
}
