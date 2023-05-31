// PARAM: --set ana.activated[+] "'region'"
#include<stdlib.h>
#include "racemacros.h"

struct s {
  int datum;
  struct s *next;
} *buckets[256];

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

pthread_mutex_t mutexes[256];


void *t1_fun(void *arg) {
  struct s *p = malloc(sizeof(struct s)); init(p);
  int i = __VERIFIER_nondet_int() % 256;
  insert(p, buckets[i]);
  return NULL;
}

void *t2_fun(void *arg) {
  int i = __VERIFIER_nondet_int() % 256;
  int j = __VERIFIER_nondet_int() % 256;
  struct s *p = take(buckets[j]);
  pthread_mutex_lock(&mutexes[i]);
  access_or_assert_racefree(p->datum); // UNKNOWN
  pthread_mutex_unlock(&mutexes[i]);
  return NULL;
}

int main () {
  for (int i = 0; i < 256; i++) {
    pthread_mutex_init(&mutexes[i], NULL);
    struct s *p =malloc(sizeof(struct s)); init(p);
    buckets[i] = p;
  }

  create_threads(t1);
  create_threads(t2);
  join_threads(t1);
  join_threads(t2);
  return 0;
}
