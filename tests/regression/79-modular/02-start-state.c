//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
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



