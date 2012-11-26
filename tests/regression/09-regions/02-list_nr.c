// PARAM: --set ana.activated[0][+] "'region'" 
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct s {
  int datum;
  struct s *next;
} *A;

void init (struct s *p, int x) {
  p -> datum = x;
  p -> next = NULL;
}

pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  struct s *p = malloc(sizeof(struct s));
  struct s *t;
  init(p,7);
  
  pthread_mutex_lock(&A_mutex);
  t = A->next; // NORACE
  A->next = p; // NORACE
  p->next = t; // NORACE
  pthread_mutex_unlock(&A_mutex);
  return NULL;
}

int main () {
  pthread_t t1;
  struct s *p = malloc(sizeof(struct s));
  init(p,9);

  A = malloc(sizeof(struct s));
  init(A,3);
  A->next = p;

  pthread_create(&t1, NULL, t_fun, NULL);
  
  pthread_mutex_lock(&A_mutex);
  p = A->next; // NORACE
  printf("%d\n", p->datum); // NORACE
  pthread_mutex_unlock(&A_mutex);
  return 0;
}
