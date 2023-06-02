//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
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

	n.next = &n;

	__goblint_check(n.next != &m);
	__goblint_check(n.next != &o);
	__goblint_check(n.next != NULL);

	prepend(&n, &m);
	__goblint_check(n.next != &m); //UNKNOWN
	__goblint_check(n.next != m.next); //UNKNOWN
	__goblint_check(n.next != NULL); //UNKNOWN
	__goblint_check(n.next != &o);

}

void prepend2(node_t *hd, node_t *tail){
	if(hd != NULL){
		if(tail != NULL){
			hd->next = tail;
		}
	}
}

void call_prepend2(){
	node_t n = {0};
	node_t m = {0};
	node_t o = {0};
	n.next = &n;

	__goblint_check(n.next != &m);
	__goblint_check(n.next != &o);
	__goblint_check(n.next != NULL);

	prepend2(&n, &m);
	__goblint_check(n.next != &m); //UNKNOWN
	__goblint_check(n.next != m.next); //UNKNOWN
	__goblint_check(n.next != &o);
	__goblint_check(n.next != NULL);
}
