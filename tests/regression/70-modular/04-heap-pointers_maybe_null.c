//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'"
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

