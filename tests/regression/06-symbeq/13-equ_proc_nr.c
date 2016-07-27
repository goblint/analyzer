// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"  
#include<pthread.h>
#include<stdlib.h>

struct s {
  int datum;
  pthread_mutex_t mutex;
} A, B;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A.mutex);
  A.datum = 5; //NORACE
  pthread_mutex_lock(&A.mutex);
  return NULL;
}

void update(int *p) {
  *p = 8; //NORACE
}

int main () {
  int x;
  pthread_t id;

  struct s *s;
  int *d;

  pthread_mutex_t *m;

  if (x) 
    s = &A; 
  else 
    s = &B;

  m = &s->mutex;
  d = &s->datum;

  pthread_create(&id,NULL,t_fun,NULL);

  pthread_mutex_lock(m);
  update(d);
  pthread_mutex_unlock(m);

  return 0;
}
