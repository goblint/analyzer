#include <pthread.h>

int g;
int *gp = &g;

void *t_fun(void *arg) {
  g = 1; // NORACE
  gp = NULL; // NORACE
  return NULL;
}

// The operand of sizeof and __alignof__ is not evaluated, so nothing in it is read.
int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  size_t s1 = sizeof(g); // NORACE
  size_t s2 = sizeof(*gp); // NORACE
  size_t s3 = __alignof__(g); // NORACE
  pthread_join(id, NULL);
  return 0;
}
