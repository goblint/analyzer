// PARAM: --enable annotation.goblint_relation_track --set ana.activated[+] apron --set ana.activated[+] memLeak --set ana.ctx_insens "['base','mallocWrapper','mutexEvents','assert','apron','memLeak']" --set ana.malloc.unique_address_count 2

// Adapted from https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/c/list-properties/list_search-1.c
#include <goblint.h>

typedef struct thing {
	int key;
} thing;

thing *container;

int insert(int k __attribute__((__goblint_relation_track__))) {
  container = (thing*) malloc(sizeof(thing));
  container->key = k;
  return 0;
}

int main(void){
  insert(2);
  insert(5);

  if(container !=0) { // Used to detect dead code after loop head

  }

  __goblint_check(1); // Should be reachable
  return 0;
}
