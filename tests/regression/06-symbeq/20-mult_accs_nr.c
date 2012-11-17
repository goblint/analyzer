// PARAM: --set ana.activated[0][+] "'var_eq'"  --set ana.activated[0][+] "'symb_locks'"  
#include<pthread.h>

struct s {
  int data;
  int lore;
  pthread_mutex_t mutex;
};

extern struct s *get_s();

void *t_fun(void *arg) {
  struct s *s; 
  s = get_s();
  pthread_mutex_lock(&s->mutex);
  s->data = 5; // NORACE
  s->lore = 6; // NORACE
  pthread_mutex_lock(&s->mutex);
  return NULL;
}

int main () {
  int *d, *l;
  struct s *s;
  pthread_t id;
  pthread_mutex_t *m;

  s = get_s();
  m = &s->mutex;
  d = &s->data;
  l = &s->lore;

  pthread_create(&id,NULL,t_fun,NULL);

  pthread_mutex_lock(m);
  *d = 8; // NORACE
  *l = 9; // NORACE
  pthread_mutex_unlock(m);

  return 0;
}
