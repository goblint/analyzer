//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

typedef struct node {
	struct node * next;
	int value;
} node_t;

void prepend(node_t *hd, node_t *tail){
	if(hd != NULL){
		hd->next = tail;
	}
}

void call_prepend(){
	node_t n = {0};
	node_t m = {0};
	node_t o = {0};

	__goblint_check(n.next != &m);
	__goblint_check(n.next != &o);
	prepend(&n, &m);
	__goblint_check(n.next != &m); //UNKNOWN
	__goblint_check(n.next != m.next); //UNKNOWN
	__goblint_check(n.next != &o);

}