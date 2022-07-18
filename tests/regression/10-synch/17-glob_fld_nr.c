// PARAM: --set ana.activated[+] thread
// Copy of 05/08 with thread enabled
#include <pthread.h>

struct {
  int x;
  int y;
} data;

void *t_fun(void *arg) {
  data.x++; // NORACE
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  data.y++; // NORACE
  return 0;
}

