// PARAM: --disable ana.int.def_exc_widen_by_join
// NOTIMEOUT: Used to timeout without ana.int.def_exc_widen_by_join
#include<pthread.h>

struct a {
  int b;
};

void c(struct a *g) {
  int x;
  struct a *e;
  while (x)
    e = &g[g->b];
}

void *f(void *arg) {
  struct a d;
  c(&d);
  return NULL;
}

int main() {
  pthread_t t;
  pthread_create(&t, NULL, f, NULL);
  return 0;
}
