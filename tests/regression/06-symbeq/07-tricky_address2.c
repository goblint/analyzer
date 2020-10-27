// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
extern int __VERIFIER_nondet_int();
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

#include<pthread.h>
#include<stdio.h>

struct s {
  int datum;
  pthread_mutex_t mutex;
} a[10];

void *t_fun(void *arg) {
  int i = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= i && i < 10);
  struct s *p = &a[i];
  pthread_mutex_lock(&p->mutex);
  a[i].datum++; // NORACE
  pthread_mutex_unlock(&p->mutex);
  return NULL;
}

int main () {
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&a[i].mutex, NULL);

  int i = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= i && i < 10);
  pthread_t t1;
  pthread_create(&t1, NULL, t_fun, NULL);

  pthread_mutex_lock(&a[i].mutex);
  a[i].datum++; // NORACE
  pthread_mutex_unlock(&a[i].mutex);
  return 0;
}
