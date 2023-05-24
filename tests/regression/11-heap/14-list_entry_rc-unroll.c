// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --set ana.malloc.unique_address_count 1
// Copied from 06-symbeq/14-list_entry_rc, proven safe thanks to unique address
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

#define list_entry(ptr, type, member) \
  ((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))

pthread_mutexattr_t mutexattr;

struct s {
  int datum;
  pthread_mutex_t mutex;
  int list;
} *A;

void init (struct s *p, int x) {
  p->datum = x;
  pthread_mutex_init(&p->mutex, &mutexattr);
}

void update (int *p) {
  struct s *s = list_entry(p, struct s, list);
  pthread_mutex_lock(&s->mutex);
  s++;
  // Not actual race: https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/issues/1354
  s->datum++; // NORACE
  pthread_mutex_unlock(&s->mutex); // no UB because ERRORCHECK
}

void *t_fun(void *arg) {
  update(&A->list);
  return NULL;
}

int main () {
  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_ERRORCHECK);

  pthread_t t1;
  A = malloc(2 * sizeof(struct s));
  init(A,666);
  init(&A[1],999); // extra element for s++ in update

  pthread_create(&t1, NULL, t_fun, NULL);
  update(&A->list);
  return 0;
}

