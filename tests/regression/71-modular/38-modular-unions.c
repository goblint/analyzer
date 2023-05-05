//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <goblint.h>

extern int __VERIFIER_nondet_int(void);

int g = 0;

typedef struct node {
	int value;
	struct node* next;
} node_t;

typedef union {
	int *iptr;
	node_t *nptr;
} int_or_node;

void *write_node(int_or_node u, int is_node){
	if(is_node){
		u.nptr->value = 23;
	} else {
		*(u.iptr) = 12;
	}
}

int main(){
	int is_node = __VERIFIER_nondet_int();
	int_or_node ptr = {0};
	node_t n = {0};
	int i = 0;

	if(is_node){
		ptr.nptr = &n;
	} else {
		ptr.iptr = &i;
	}

	write_node(ptr, is_node);

	if(is_node){
		__goblint_check(ptr.nptr->value <= 23);
		__goblint_check(ptr.nptr->value == 23); // UNKNOWN
	} else {
		__goblint_check(ptr.iptr <= 12);
		__goblint_check(ptr.iptr == 12); // UNKNOWN
	}

	return 0;
}