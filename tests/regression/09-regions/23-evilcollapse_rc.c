// PARAM: --set ana.activated[+] "'symb_locks'" --set ana.activated[+] "'region'" --set exp.region-offsets true --sets solver wpoint
#include<stdlib.h>

struct list_head {
  struct list_head *next ;
  struct list_head *prev ;
};

struct list_head c[10] ;

static void INIT_LIST_HEAD(struct list_head *list) {
  list->next = list;
  list->prev = list;
}

struct list_head *new(int x) {
  struct list_head *p = malloc(sizeof(struct list_head));
  INIT_LIST_HEAD(p);
  return p;
}

static void list_add(struct list_head *new, struct list_head *head) {
  struct list_head *next = head->next;
  next->prev = new;
  new->next = next;
  new->prev = head;
  head->next = new;
}

static struct list_head *lookup (int d) {
  int hvalue;
  struct list_head *p;
  p = c[hvalue].next;
  return p;
}

int main() {
  struct list_head *p1, *p2;
  for (int i = 0; i < 10; i++) {
    INIT_LIST_HEAD(&c[i]);
    for (int j = 0; j < 3; j++) {
      struct list_head *arg1, *arg2;
      arg1 = new(j*i);
      arg2 = &c[i];
      list_add(arg1, arg2);
    }
  }
  p1 = lookup(1);
  p2 = lookup(2);
  p1->next = p2->next;
  // per-element scheme no longer safe.
  return 0;
}
