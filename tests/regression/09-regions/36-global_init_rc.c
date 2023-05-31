// PARAM: --set ana.activated[+] "'region'"
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct s {
  int datum;
  struct s *next;
};

struct s p = {9, NULL};
struct s A = {3, &p};
struct s B = {5, &p};


pthread_mutex_t A_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B_mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A_mutex);
  A.next->datum++; // RACE!
  pthread_mutex_unlock(&A_mutex);

  pthread_mutex_lock(&B_mutex);
  B.next->datum++; // RACE!
  pthread_mutex_unlock(&B_mutex);
  return NULL;
}

int main () {
  pthread_t t1;
  struct s *p;

  pthread_create(&t1, NULL, t_fun, NULL);

  pthread_mutex_lock(&A_mutex);
  p = A.next;
  printf("%d\n", p->datum); // RACE!
  pthread_mutex_unlock(&A_mutex);

  pthread_mutex_lock(&B_mutex);
  p = B.next;
  printf("%d\n", p->datum); // RACE!
  pthread_mutex_unlock(&B_mutex);
  return 0;
}
