#include <pthread.h>
#include <stdio.h>

int g, g1, g2, g3, g4;

struct q{
  int i;
};

struct s {
  int *p;
  int n;
  struct q * qp;
};

extern void mystery_fn(int *p);
extern int* rantom();

void *t_fun(void *arg) {
  int uk;
  int *p = &g2;
  if (uk) p = (int)rantom();

  struct s sx;
  sx.p  = &g3;
  sx.qp = rantom();

  // This regression test is here to document
  // the confidence levels of accesses:

  g      = 10;              // syntactically unambigous access:            RACE @ conf. 110
  *(&g1) = 20;              // through a P.T.-set with no unknown element: RACE @ conf. 110
  *p     = 20;              // through a P.T.-set with an unknown element: RACE @ conf. 100
  mystery_fn((int *)&sx);   // through possible reachability:              RACE @ conf. 80
                            //           additionally: (struct q).i gets a race @ conf. 60

  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  return 0;
}
