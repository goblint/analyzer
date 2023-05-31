// PARAM: --set ana.activated[+] "'region'"  --set exp.region-offsets true
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include "racemacros.h"

struct s {
  int datum;
  struct s *next;
} *A;

void init (struct s *p, int x) {
  p -> datum = x;
  p -> next = NULL;
}

void insert(struct s *p, struct s **list) {
  p->next = *list;
  *list = p;
}

pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  struct s *p = malloc(sizeof(struct s));
  init(p,7);

  pthread_mutex_lock(&A_mutex);
  insert(p, &A);
  pthread_mutex_unlock(&A_mutex);

  pthread_mutex_lock(&B_mutex);
  access_or_assert_racefree(p->datum); // UNKNOWN
  pthread_mutex_unlock(&B_mutex);
  return NULL;
}

int main () {
  struct s *p;
  A = malloc(sizeof(struct s));
  init(A,3);

  create_threads(t);

  p = malloc(sizeof(struct s));
  init(p,9);

  pthread_mutex_lock(&A_mutex);
  insert(p, &A);
  pthread_mutex_unlock(&A_mutex);

  pthread_mutex_lock(&A_mutex);
  p = A;
  while (p->next) {
    access_or_assert_racefree(p->datum); // UNKNOWN
    p = p->next;
  }
  pthread_mutex_unlock(&A_mutex);

  join_threads(t);
  return 0;
}
