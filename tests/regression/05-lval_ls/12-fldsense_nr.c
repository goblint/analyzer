#include <pthread.h>

struct { int x; int y; } data;
struct { pthread_mutex_t x; pthread_mutex_t y; } m;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m.x);
  data.x++; // NORACE
  pthread_mutex_unlock(&m.x);
  return NULL;
}

int main() {
  pthread_mutex_init(&m.x, NULL);
  pthread_mutex_init(&m.y, NULL);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m.x);
  data.x++; // NORACE
  pthread_mutex_unlock(&m.x);
  pthread_mutex_lock(&m.y);
  data.y++; // NORACE
  pthread_mutex_unlock(&m.y);
  return 0;
}

