// PARAM: --set ana.activated[+] regionNonEscape --set ana.activated[-] mhp --set ana.thread.domain plain
#include <pthread.h>
#include <stdlib.h>

struct s {
  struct s *next;
};

struct s *g = NULL;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  struct s *r = g->next; // RACE!
  pthread_mutex_unlock(&A);
  return NULL;
}

struct s* alloc_s() {
  return malloc(sizeof(struct s));
}

int main() {
  struct s *p, *q;
  int r;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  p = alloc_s();
  q = p;
  // p->next = NULL; // RACE!
  pthread_mutex_lock(&A);
  g = p; // NORACE
  pthread_mutex_unlock(&A);

  // p and q point to same blob, so accessing via both should race
  if (r)
    p->next = NULL; // RACE!
  else
    q->next = NULL; // RACE!
  return 0;
}