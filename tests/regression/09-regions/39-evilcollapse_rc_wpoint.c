// FIXPOINT issue with some old solvers: https://github.com/goblint/analyzer/pull/66
// Currently also with topdown_term and topdown_space_cache_term
#include<stdlib.h>

struct list_head {
  struct list_head *next ;
  struct list_head *prev ;
};

struct list_head c[10] ;

static void INIT_LIST_HEAD(struct list_head *list) {
  return;
}

static struct list_head *lookup () {
  int i = 0;
  return c[i].next;
}

int main() {
  struct list_head *p1, *p2;
  INIT_LIST_HEAD(&c[0]);
  for ( ; 0; ) { }
  p1 = lookup();
  p1->next = p2;
  return 0;
}
