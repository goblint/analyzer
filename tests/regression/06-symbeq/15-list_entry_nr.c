// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

#define list_entry(ptr, type, member) \
  ((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))

struct s {
  int datum;
  pthread_mutex_t mutex;
  int list;
} *A;

void init (struct s *p, int x) {
  p->datum = x;
  pthread_mutex_init(&p->mutex, NULL);
}

void update (int *p) {
  struct s *s = list_entry(p, struct s, list);
  pthread_mutex_lock(&s->mutex);
  s->datum++; // NORACE
  pthread_mutex_unlock(&s->mutex);
}

void *t_fun(void *arg) {
  update(&A->list);
  return NULL;
}

int main () {
  pthread_t t1;
  A = malloc(sizeof(struct s));
  init(A,666);

  pthread_create(&t1, NULL, t_fun, NULL);
  update(&A->list);
  return 0;
}

