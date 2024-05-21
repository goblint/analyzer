// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
#include <goblint.h>
#include <stdlib.h>

struct list {
  int data;
  struct list *next;
};

void main(void) {
  struct list last = {
    41
  };
  struct list first = {
    42, &last
  };

  last.next = &last;

  __goblint_check(first.next->next->next->next == &last);
}
