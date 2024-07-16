#include <stdio.h>
#include <goblint.h>

extern void *malloc(size_t);
extern int putchar(int);


struct ListElm {
  char cAtom;
  struct ListElm *next;
};

typedef struct ListElm ListElm;

main () {
  ListElm *head, *tail, *temp;
  head = (ListElm *)malloc(sizeof(ListElm));
  head -> cAtom = (char)0;
  head -> next = 0;
  tail = head;
  while (tail -> cAtom != 'x') {
    temp = (ListElm *)malloc(sizeof(ListElm));
    temp -> cAtom = (char)((tail -> cAtom) + 1);
    putchar(temp -> cAtom);
    temp -> next = 0;
    tail -> next = temp;
    tail = tail -> next;
  }
  printf("That's all.\n");
  __goblint_check(head->cAtom == 0);
  __goblint_check(tail->cAtom != 0); //TODO
  printf("1. %c\n", head -> cAtom);
  printf("2. %c\n", tail -> cAtom);
  return 0;
}
