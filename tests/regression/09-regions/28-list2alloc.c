// PARAM: --set ana.activated[+] "'region'"
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct s {
  int datum;
  struct s *next;
} *A, *B;

struct s *new(int x) {
  struct s *p = malloc(sizeof(struct s));
  p->datum = x;
  p->next = NULL;
  return p;
}

pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A_mutex);
  A->datum++; // RACE <-- this line is also relevant.
  pthread_mutex_unlock(&A_mutex);

  pthread_mutex_lock(&B_mutex);
  B->datum++; // <-- this is not relevant at all.
  pthread_mutex_unlock(&B_mutex);
  return NULL;
}

int main () {
  pthread_t t1;

  A = new(3);
  B = new(5);

  pthread_create(&t1, NULL, t_fun, NULL);

  int *data;
  pthread_mutex_lock(&A_mutex);
  data = &A->datum; // NORACE
  pthread_mutex_unlock(&A_mutex);
  *data = 42; // RACE <-- this is the real bug!
  return 0;
}
