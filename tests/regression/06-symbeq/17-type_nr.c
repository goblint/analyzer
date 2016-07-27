// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"  
#include<pthread.h>

struct s {
  int datum;
  pthread_mutex_t mutex;
};

extern struct s *get_s();

void *t_fun(void *arg) {
  struct s *s; 
  s = get_s();
  pthread_mutex_lock(&s->mutex);
  s->datum = 5; // NORACE
  pthread_mutex_lock(&s->mutex);
  return NULL;
}

int main () {
  int *d;
  struct s *s;
  pthread_t id;
  pthread_mutex_t *m;

  s = get_s();
  m = &s->mutex;
  d = &s->datum;

  pthread_create(&id,NULL,t_fun,NULL);

  pthread_mutex_lock(m);
  *d = 8; // NORACE
  pthread_mutex_unlock(m);

  return 0;
}
