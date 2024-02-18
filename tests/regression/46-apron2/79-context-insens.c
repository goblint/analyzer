// PARAM: --set ana.activated "['base', 'threadid', 'threadflag', 'threadreturn','mallocWrapper','mutexEvents','mutex','access','race','escape','expRelation','assert','var_eq','symb_locks','apron','memLeak']" --enable ana.int.interval_set --set ana.ctx_insens "['base', 'threadid', 'threadflag', 'threadreturn','mallocWrapper','mutexEvents','mutex','access','race','escape','expRelation','assert','var_eq','symb_locks','apron','memLeak']" --enable ana.autotune.enabled --set ana.autotune.activated "['octagon']" --set ana.specification "CHECK( init(main()), LTL(G valid-memcleanup) )" --set ana.path_sens "['mutex', 'malloc_null', 'uninit','expsplit','activeSetjmp','memLeak',      'threadflag']" --set ana.malloc.unique_address_count 5

// Adapted from https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/list-properties/list_search-1.c
#include <stdlib.h>
#include <assert.h>
#include <goblint.h>

typedef struct list {
	int key;
	struct list *next;
} mlist;

mlist *head;

mlist* search(mlist *l, int k){
  l = head;
  while(l!=0 && l->key!=k) { // Used to detect dead code after loop head
    l = l->next;
  }
  return l;
}

int insert(mlist *l, int k){
  l = (mlist*)malloc(sizeof(mlist));
  l->key = k;
  if (head==0) {
  } else {
    l->key = k;
    l->next = head;
  }
  head = l;
  return 0;
}

int main(void){
  int i;
  mlist *mylist, *temp;
  insert(mylist,2);
  insert(mylist,5);

  temp = search(head,2);

  __goblint_check(1); // Should be reachable
  return 0;
}

