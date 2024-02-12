//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
#include <stdlib.h>

int foo(int *p){
	*p = 23;
	return 0;
}

struct node {
	struct node* next;
	int value;
};

int modify_node_ptr(struct node** n){
	*n = NULL;
	return 0;
}

int modify_node_ptr_indirect(struct node** n){
	(*n)->next = NULL;
	return 0;
}


int modify_node(struct node* n){
	n->next = NULL;
	return 0;
}

int modify_node_indirect(struct node* n){
	n->next->next = NULL;
	return 0;
}