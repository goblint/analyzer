// PARAM: --set ana.activated[0][+] "'shape'" 
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct list_head {
  struct list_head *next, *prev;
};

struct node {
  int datum;
  struct list_head list;
}; 

struct list_head A, B;

#define list_entry(ptr, type, member) 			\
  (type *)( (char *)ptr - __builtin_offsetof(type,member) )

pthread_mutex_t mutex_A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex_B = PTHREAD_MUTEX_INITIALIZER;

void *generate(void *arg) {
  int i;
  struct list_head *l, *an;
  struct node *n; 
  for (i=1; i<100; i++) {
    n = malloc(sizeof(struct node));

    // INIT_LIST_HEAD(&n->list);
    l = &n->list; // NORACE
    l->next = l; // NORACE
    l->prev = l; // NORACE

    n->datum = i; // NORACE

    pthread_mutex_lock(&mutex_A);
    // list_add(&n->list, &A); 
    an = A.next;
    an->prev = l;
    l->next = an;
    l->prev = &A;
    A.next = l;

    pthread_mutex_unlock(&mutex_A);
    sleep(1);
  }
  return NULL;
}

void *process(void *arg) {
  struct list_head *l, *ln, *lp, *bn;
  while (1) {
    pthread_mutex_lock(&mutex_A);
    // ! empty(&A)
    if (A.next != &A) {
      l = A.next;
      pthread_mutex_lock(&mutex_B);
      //list_move(l,&B);
      //  remove from A
      ln = l->next;
      lp = l->prev;
      ln->prev = lp;
      lp->next = ln;
      //  add to B
      bn = B.next;
      bn->prev = l;
      l->next = bn;
      l->prev = &B;
      B.next = l;

      pthread_mutex_unlock(&mutex_B);
      pthread_mutex_unlock(&mutex_A);
    }
    else
      pthread_mutex_unlock(&mutex_A);
    sleep(2);
  }
  return NULL;
}

void *dispose(void *arg) {
  struct list_head *l, *ln, *lp;
  struct node *n;
  while (1) {
    pthread_mutex_lock(&mutex_B);
    if (B.next != &B) {
      l = B.next;
      // list_del(l);
      ln = l->next;
      lp = l->prev;
      ln->prev = l;
      lp->next = l;
      l->next = NULL;
      l->prev = NULL;
      pthread_mutex_unlock(&mutex_B);
      n = list_entry(l, struct node, list);
      printf("Data disposed: %d\n", n->datum);
      n->datum *= -1; //RACE
    }
    else
      pthread_mutex_unlock(&mutex_B);
    sleep(5);
  }
  return NULL;
}

int main () {
  pthread_t t1, t2, t3;
  int i;
  struct node *n;

  // INIT_LIST_HEAD(&A);
  A.next = &A; A.prev = &A;
  // INIT_LIST_HEAD(&B);
  B.next = &B; B.prev = &B;

  pthread_create(&t1, NULL, generate, NULL);
  pthread_create(&t2, NULL, process, NULL);
  pthread_create(&t3, NULL, dispose, NULL);


  // THE REST IS JUST FOR TESTING 
  
#define list_for_each_entry(pos, head, member)				\
	for (pos = list_entry((head)->next, typeof(*pos), member);	\
	     &pos->member != (head); 	\
	     pos = list_entry(pos->member.next, typeof(*pos), member))
  for (i=0; i<10; i++) {
    pthread_mutex_lock(&mutex_A);
    pthread_mutex_lock(&mutex_B);

    printf("List A: ");
    list_for_each_entry(n, &A, list)
      printf("%d ", n->datum);

    printf("\nList B: ");
    list_for_each_entry(n, &B, list)
      printf("%d ", n->datum);
    printf("\n");

    pthread_mutex_unlock(&mutex_B);
    pthread_mutex_unlock(&mutex_A);
    sleep(3);
  }
  return 0;
}

