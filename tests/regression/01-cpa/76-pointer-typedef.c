// PARAM: --set ana.malloc.unique_address_count 2
// Extracted (using creduce) from SV-COMP task list-simple/dll2c_remove_all.i
#include <stdlib.h>
#include <goblint.h>

typedef struct node {
  struct node *next;
  struct node *prev;
} * DLL;

void dll_remove(DLL *head) {
  DLL temp = (*head)->next;
  if (temp == *head) {
    __goblint_check(temp == *head);
    __goblint_check(temp != *head); // FAIL
    free(*head);
  }
  else {
    __goblint_check(temp != *head);
    __goblint_check(temp == *head); // FAIL
    (*head)->prev->next = temp;
    free(*head);
    *head = temp;
  }
}
main() {
  DLL s = malloc(sizeof(struct node));
  s->next = s->prev = malloc(sizeof(struct node));

  dll_remove(&s);
  dll_remove(&s);
}
