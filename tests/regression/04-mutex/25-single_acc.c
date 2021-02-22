#include <pthread.h>

int x;

void *t_fun(void *arg) {
  x++; // RACE!
  return NULL;
}

int main() {
  pthread_t id1, id2;

  pthread_create(&id1, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);

  return 0;
}
