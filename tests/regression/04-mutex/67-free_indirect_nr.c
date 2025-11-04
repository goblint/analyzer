// PARAM: --disable ana.race.free
#include <pthread.h>
#include <stdlib.h>

struct s {
  int i;
};

void *t_fun(void *arg) {
  struct s *p = (struct s *) arg;
  p->i++; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  struct s *p = malloc(sizeof(struct s));
  pthread_create(&id, NULL, t_fun, (void *) p);
  free(p); // NORACE
  pthread_join (id, NULL);
  return 0;
}
