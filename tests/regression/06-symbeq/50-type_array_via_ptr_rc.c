// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
#include<pthread.h>

struct s {
  int datum[2];
  int datums[2][2][2];
  pthread_mutex_t mutex;
};

extern struct s *get_s();

void *t_fun(void *arg) {
  struct s *s = get_s();
  s->datum[1] = 5; // RACE!
  s->datums[1][1][1] = 5; // RACE!
  return NULL;
}

int main () {
  int *d, *e;
  struct s *s;
  pthread_t id;
  pthread_mutex_t *m;

  s = get_s();
  m = &s->mutex;
  d = &s->datum[1];
  e = &s->datums[1][1][1];

  pthread_create(&id,NULL,t_fun,NULL);
  *d = 8; // RACE!
  *e = 8; // RACE!

  return 0;
}
