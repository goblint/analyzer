// PARAM: --set ana.activated[+] "'region'"  --set exp.region-offsets true
extern int __VERIFIER_nondet_int();
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct s {
  int datum;
  struct s *next;
};

struct cache {
  struct s       *slots[10];
  pthread_mutex_t mutex[10];
} c;

struct s *new(int x) {
  struct s *p = malloc(sizeof(struct s));
  p->datum = x;
  p->next = NULL;
  return p;
}

void list_add(struct s *node, struct s *list) {
  struct s *temp = list->next;
  list->next = node;
  node->next = temp;
}

void *t_fun(void *arg) {
  int i = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= i && i < 9);
  pthread_mutex_lock(&c.mutex[i+1]);
  list_add(new(3), c.slots[i]);
  pthread_mutex_unlock(&c.mutex[i+1]);
  return NULL;
}

int main () {
  int j = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= j && j < 10);
  struct s *p;
  pthread_t t1;

  c.slots[j] = new(1);
  list_add(new(2), c.slots[j]);

  pthread_create(&t1, NULL, t_fun, NULL);

  pthread_mutex_lock(&c.mutex[j]);
  p = c.slots[j]->next; // RACE!
  printf("%d\n", p->datum);
  pthread_mutex_unlock(&c.mutex[j]);
  return 0;
}
