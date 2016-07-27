// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"  
#include<pthread.h>
#include<stdio.h>

struct s {
  int datum;
  pthread_mutex_t mutex;
} a[10];

void *t_fun(void *arg) {
  int i;
  struct s *p = &a[i];
  pthread_mutex_lock(&p->mutex);
  a[i].datum++; // NOWARN
  pthread_mutex_unlock(&p->mutex);
  return NULL;
}

int main () {
  int i;
  pthread_t t1;
  pthread_create(&t1, NULL, t_fun, NULL);
  
  pthread_mutex_lock(&a[i].mutex);
  a[i].datum++; // NOWARN
  pthread_mutex_unlock(&a[i].mutex);
  return 0;
}
