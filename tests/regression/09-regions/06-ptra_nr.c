// PARAM: --set ana.activated[+] "'region'" 
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

#define list_entry(ptr, type, member) \
  ((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))

struct s {
  struct s *next;
  int datum;
} *A, *B;

void init (struct s *p, int x) {
  p -> datum = x;
  p -> next = NULL;
}

pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *ip;
  struct s *t, *sp;
  struct s *p = malloc(sizeof(struct s));
  init(p,7);
  
  ip = &p->datum;
  sp = list_entry(ip, struct s, datum);

  pthread_mutex_lock(&A_mutex);
  t = A->next;  // NORACE
  A->next = sp; // NORACE
  sp->next = t; // NORACE
  pthread_mutex_unlock(&A_mutex);
  return NULL;
}

int main () {
  pthread_t t1;
  int *ip;
  struct s *sp;
  struct s *p = malloc(sizeof(struct s));
  init(p,9);

  A = malloc(sizeof(struct s));
  init(A,3);
  A->next = p;
  B = malloc(sizeof(struct s));
  init(B,5);

  pthread_create(&t1, NULL, t_fun, NULL);
  
  ip = &p->datum;
  sp = list_entry(ip, struct s, datum);
  
  pthread_mutex_lock(&A_mutex);
  sp = A->next; // NORACE
  printf("%d\n", p->datum); // NORACE
  pthread_mutex_unlock(&A_mutex);
  return 0;
}
