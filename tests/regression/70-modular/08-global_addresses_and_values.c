//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'"
#include<goblint.h>
#include<stdlib.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t g;
node_t *h;

// Check that modular analysis treats addresses and values of global variables soundly
int update_through_parameter_pointer(node_t* n){
	__goblint_check(h == n); //UNKNOWN!
	__goblint_check(&g == n); //UNKNOWN!
	__goblint_check(&g == h); //UNKNOWN!

	node_t *m = malloc(sizeof(node_t));
	m->next = NULL;

	__goblint_check(n->next != m);
	__goblint_check(g.next != m);
	__goblint_check(h->next != m);
	__goblint_check(n != m);
	__goblint_check(h != m);

	n->next = m;
	__goblint_check(n->next != m); //UNKNOWN! (May have been changed by other thread)
	__goblint_check(g.next != m); //UNKNOWN!
	__goblint_check(h->next != m); //UNKNOWN!
	__goblint_check(n != m);
	__goblint_check(h != m);

}

int update_through_global(node_t* n){
	__goblint_check(h == n); //UNKNOWN!
	__goblint_check(&g == n); //UNKNOWN!
	__goblint_check(&g == h); //UNKNOWN!

	node_t *m = malloc(sizeof(node_t));
	m->next = NULL;

	__goblint_check(n->next != m);
	__goblint_check(g.next != m);
	__goblint_check(h->next != m);
	__goblint_check(n != m);
	__goblint_check(h != m);

	g.next = m;

	__goblint_check(g.next != m); //UNKNOWN! (May have been changed by other thread)
	__goblint_check(n->next != m); //UNKNOWN!
	__goblint_check(h->next != m); //UNKNOWN!
	__goblint_check(n != m);
	__goblint_check(h != m);
}

int update_through_global_pointer(node_t* n){
	__goblint_check(h == n); //UNKNOWN!
	__goblint_check(&g == n); //UNKNOWN!
	__goblint_check(&g == h); //UNKNOWN!

	node_t *m = malloc(sizeof(node_t));
	m->next = NULL;

	__goblint_check(n->next != m);
	__goblint_check(g.next != m);
	__goblint_check(h->next != m);
	__goblint_check(n != m);
	__goblint_check(h != m);

	h->next = m;

	__goblint_check(g.next != m); //UNKNOWN! (May have been changed by other thread)
	__goblint_check(n->next != m); //UNKNOWN!
	__goblint_check(h->next != m); //UNKNOWN!
	__goblint_check(n != m);
	__goblint_check(h != m);
}
