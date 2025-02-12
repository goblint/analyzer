// PARAM: --set ana.activated[+] memOutOfBounds --set exp.architecture 32bit --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G valid-deref) )"
// Minimized version of SV-COMP task list-simple/dll2c_append_equal.i
#include <stdlib.h>

typedef struct node {
  struct node *next;
  struct node *prev;
  int data;
} *DLL;

int main(void) {
  DLL temp = (DLL) malloc(sizeof(struct node));
  temp->next = NULL; // NOWARN
  temp->prev = NULL; // NOWARN
  temp->data = 1;    // NOWARN
  return temp;
}