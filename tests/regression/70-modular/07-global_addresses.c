//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'"
#include<goblint.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t g;
node_t *h;

// Check that modular analysis treats addresses of globals soundly
int foo(node_t* n){
	__goblint_check(h == n); //UNKNOWN!
	__goblint_check(&g == n); //UNKNOWN!
	__goblint_check(&g == h); //UNKNOWN!

	node_t *x = &g;
	node_t *y = h;
	__goblint_check(x == y); //UNKNOWN!
}
