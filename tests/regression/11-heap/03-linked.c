#include <assert.h>

struct list
{
    int n;
    struct list *next;
};

void *allocate_memory()
{
    return malloc(8U);
}

struct list *append(struct list *l, int n)
{
    struct list *new_el;

    new_el = allocate_memory();

    new_el->n = n;
    new_el->next = l;

    return new_el;
}

int main()
{
    struct list *l, m;
    l = &m;
    l->next = 0;
    l->n = 0;

    l = append(l, 1);
    l = append(l, 2);

    __goblint_check(l->next->next->n == 0); //UNKNOWN
}
