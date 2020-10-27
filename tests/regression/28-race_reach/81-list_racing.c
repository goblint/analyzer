// PARAM: --set ana.activated[+] "'region'"
#include<stdlib.h>
#include "racemacros.h"

struct s {
  int datum;
  struct s *next;
} *A;

void init (struct s *p) {
  p -> datum = 0;
  p -> next = NULL;
}

// Naive implementation of synchronized list library:
pthread_mutex_t list_mutex = PTHREAD_MUTEX_INITIALIZER;

struct s *take(struct s *list) {
  pthread_mutex_lock(&list_mutex);
  struct s *p = list;
  while (p -> next != NULL && __VERIFIER_nondet_int()) p = p -> next;
  pthread_mutex_unlock(&list_mutex);
  return p;
}

void insert(struct s *node, struct s *list) {
  pthread_mutex_lock(&list_mutex);
  struct s *t = list->next;
  list->next = node;
  node->next = t;
  pthread_mutex_unlock(&list_mutex);
}


pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t1_fun(void *arg) {
  struct s *p = malloc(sizeof(struct s));
  init(p);
  insert(p, A);
  return NULL;
}

void *t2_fun(void *arg) {
  struct s *p = take(A);
  access_or_assert_racefree(p->datum); // UNKNOWN
  return NULL;
}



int main () {
  A = malloc(sizeof(struct s)); init(A);
  create_threads(t1);
  create_threads(t2);
  join_threads(t1);
  join_threads(t2);
  return 0;
}
