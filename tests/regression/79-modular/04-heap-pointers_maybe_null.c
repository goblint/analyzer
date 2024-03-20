//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
#include <goblint.h>
#include <stdlib.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

void list_op(node_t *n){
	// Check that modular analysis does not exclude NULL pointers
	__goblint_check(n == NULL); //UNKNOWN!
	__goblint_check(n->next == NULL); //UNKNOWN!
}

void list_op2(node_t *n, node_t *m){
	// Check that modular analysis does not exclude NULL pointers
	__goblint_check(n == NULL); //UNKNOWN!
	__goblint_check(n->next == NULL); //UNKNOWN!

	__goblint_check(m == NULL); //UNKNOWN!
	__goblint_check(m->next == NULL); //UNKNOWN!
}
