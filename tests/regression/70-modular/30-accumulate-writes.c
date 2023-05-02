//PARAM: --set ana.modular.funs "['append_new']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>
#include<stdlib.h>

typedef struct node {
	int value;
	struct node* next;
} node_t;

void append_new(node_t *n){
	if(n == NULL)
		return;
	node_t *new = malloc(sizeof(node_t));
	node_t *new2 = malloc(sizeof(node_t));

	__goblint_check(new != new2);

	n->next = new;
	__goblint_check(n->next == new); //UNKNOWN
	__goblint_check(n->next != new2);

	n->next = new2;
	__goblint_check(n->next != new); //UNKNOWN
	__goblint_check(n->next == new2); //UNKNOWN
}

int main(){
	node_t n;
	append_new(&n);
}