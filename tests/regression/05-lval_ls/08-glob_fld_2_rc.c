// Copied to 10/17 with thread enabled
#include <pthread.h>

struct {
  int x;
  int y;
} data;

void *t_fun(void *arg) {
  data.x++; // RACE!
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  data.y++; // NORACE
  return 0;
}

