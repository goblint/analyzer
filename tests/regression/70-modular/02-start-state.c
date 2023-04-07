//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include <goblint.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

void list_op(node_t *n){
	// Check that modular analysis can handle access to values via pointers
	__goblint_check((n->next)->data == 0); //UNKNOWN!
	__goblint_check(n->data == 0); //UNKNOWN!
}




