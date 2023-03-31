//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'"
#include<goblint.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t g;
node_t *h;

// Check that modular analysis treats global variables soundly (via their type representation)
int foo(node_t* n){
	__goblint_check(h == n); //UNKNOWN!
	__goblint_check(&g == n); //UNKNOWN!
	__goblint_check(&g == h); //UNKNOWN!
}