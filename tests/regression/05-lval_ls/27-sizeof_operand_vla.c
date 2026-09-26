#include <pthread.h>
#include <stdlib.h>

int g[4];
int *gp = g;

void *t_fun(void *arg) {
  gp = NULL; // RACE
  return NULL;
}

// A length that is the size of a variable-length array is not an integer constant expression,
// so the first two sizeof operands below are variable-length arrays and are evaluated: they read gp.
int main(void) {
  int n = rand() % 3 + 1;
  int vla[n];
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  size_t s1 = sizeof(*(int (*)[sizeof vla])gp); // RACE
  size_t s2 = sizeof(*(int (*)[sizeof(int[n])])gp); // RACE
  size_t s3 = sizeof(*(int (*)[sizeof(int[4])])gp); // NORACE
  pthread_join(id, NULL);
  return 0;
}
