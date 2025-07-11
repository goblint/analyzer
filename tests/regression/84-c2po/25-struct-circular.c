// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <stdlib.h>
#include <goblint.h>

struct mem {
    int val;
};

struct list_node {
    int x;
    struct mem *mem;
    struct list_node *next;
};

int main() {
    struct mem *m = malloc(sizeof(*m));
    int x = ((struct mem *) m)->val;
    m->val = 100;

    struct list_node *head = malloc(sizeof(*head));

    head->x = 1;
    head->mem = m;
    head->next = head;

    __goblint_check(head->next == head);
    __goblint_check(head->next->next == head->next);
}
