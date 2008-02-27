#include <pthread.h>

struct { int x; int y; } data;
struct { pthread_mutex_t x; pthread_mutex_t y; } m;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m.x);
  data.x++; // NOWARN!
  pthread_mutex_unlock(&m.x);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m.x);
  data.x++; // NOWARN!
  pthread_mutex_unlock(&m.x);
  pthread_mutex_lock(&m.y);
  data.y++; // NOWARN!
  pthread_mutex_unlock(&m.y);
  return 0;
}

