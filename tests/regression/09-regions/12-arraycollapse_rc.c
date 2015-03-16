// PARAM: --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"  --set ana.activated[+] "'region'"  --set exp.region-offsets true
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct s {
  int datum;
  struct s *next;
};

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

pthread_mutex_t mutex[10];
struct s *slot[10];

void *t_fun(void *arg) {
  int i;
  pthread_mutex_lock(&mutex[i]);
  list_add(new(3), slot[i]);
  pthread_mutex_unlock(&mutex[i]);
  return NULL;
}

int main () {
  int j, k;
  struct s *p;
  pthread_t t1;
 
  slot[j] = new(1);
  list_add(new(2), slot[j]);

  slot[k] = new(1);
  list_add(new(2), slot[k]);

  p = new(3);
  list_add(p, slot[j]);
  list_add(p, slot[k]);

  // If it happens that i = k /= j, then it is a real race: the element in p
  // will be accessed with locks mutex[i] and mutex[j].
 
  pthread_create(&t1, NULL, t_fun, NULL);
  
  pthread_mutex_lock(&mutex[j]);
  p = slot[j]->next; // RACE
  printf("%d\n", p->datum);
  pthread_mutex_unlock(&mutex[j]);
  return 0;
}
