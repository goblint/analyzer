// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

struct node {
  int value;
  struct node *next;
};

struct node *head;

// release has neither a definition nor a library specification, and may free
// the list that the global head reaches, although no argument points into it.
extern void release(void);

int main(void) {
  struct node *n = malloc(sizeof(struct node));
  n->next = malloc(sizeof(struct node));
  head = n;
  struct node *second = n->next;
  second->value = 1; // NOWARN
  release();
  second->value = 2; // WARN
  return 0;
}
