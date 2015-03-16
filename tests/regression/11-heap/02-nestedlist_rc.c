// SKIP! PARAM: --set ana.activated[+] "'region'" 
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

struct node {
  int datum;
  struct node *next;
};

struct head {
  pthread_mutex_t lock;
  struct node *first;
  struct head *next;
} *list;

pthread_mutex_t list_lock = PTHREAD_MUTEX_INITIALIZER;

void init_node (struct node *p, int x) {
  p->datum = x;    // NORACE!
  p->next = NULL;  // NORACE!
}

void init_head (struct head *p) {
  pthread_mutex_init(&p->lock, NULL);
  p->first = NULL;  // NORACE!
  p->next = NULL;   // NORACE!
}


void *t_fun(void *arg) {
  struct node *tmp;
  struct head *h;
  int elems = 0;
  while (1) {
    pthread_mutex_lock(&list_lock);
    if (list) {
      h = list;
      pthread_mutex_unlock(&list_lock);
    } else {
      pthread_mutex_unlock(&list_lock);
      sleep(1);
      continue;
    }

    tmp = malloc(sizeof(struct node));
    init_node(tmp, ++elems);
    pthread_mutex_lock(&h->lock);
    tmp->next = h->first; // RACE!
    h->first = tmp;       // RACE!
    pthread_mutex_unlock(&h->lock);
    printf("Inserted element %d.\n", elems);
    sleep(1);
  }
  return NULL;
}

void *counter(void *arg) {
  struct node *tmp;
  struct head *h;
  int bucket, elem;
  while (1) {
    bucket = 0;
    printf("Counting: ");
    pthread_mutex_lock(&list_lock);
    h = list;
    while (h) {
      bucket++;
      elem = 0;
      tmp=h->first;
      while (tmp) {
        elem++;
        tmp = tmp->next; // RACE!
      }
      printf(" %d", elem);
      h = h->next;
    } 
    pthread_mutex_unlock(&list_lock);
    printf("\nCounted %d buckets.\n\n", bucket);
    sleep(10);
  }
  return NULL;
}

int main () {
  pthread_t t1, t2;
  struct head *tmp;
  int i;

  pthread_create(&t1, NULL, t_fun, NULL);
  pthread_create(&t2, NULL, counter, NULL);
  
  while (1) {
    getchar();
    printf("Creating new bucket!\n");
    tmp = malloc(sizeof(struct head));
    init_head(tmp);
    pthread_mutex_lock(&list_lock);
    tmp->next = list;
    list = tmp;
    pthread_mutex_unlock(&list_lock);
  }
  return 0;
}
