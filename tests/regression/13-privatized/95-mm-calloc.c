// PARAM: --set ana.base.privatization mutex-meet
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include <goblint.h>

struct a {
  void (*b)(void);
};

int g = 0;

struct a* t;
void m() {
  // Reachable!
  g = 1;
}

void* j(void* arg) {};

void main() {
  pthread_t tid;
  pthread_create(&tid, 0, j, NULL);
  t = calloc(1, sizeof(struct a));
  t->b = &m;
  struct a r = *t;
  r.b();

  __goblint_check(g ==0); //UNKNOWN!
}
