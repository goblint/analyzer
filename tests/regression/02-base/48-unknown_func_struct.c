#include <stdlib.h>
#include <stdio.h>
#include <goblint.h>


typedef struct list {
    int val;
    struct list *next;
} list_t;

void mutate_list(list_t n);
void mutate_list2(list_t* n);

int main(){
    list_t first;
    list_t second;

    first.next = &second;
    first.val = 1;
    second.next = NULL;
    second.val = 2;

    // When passing a struct to an unknown function, reachable memory should be invalidated
    mutate_list(first);
    __goblint_check(second.val == 2); //UNKNOWN!

    // Passing a pointer to the struct here.
    mutate_list2(&first);
    __goblint_check(second.val == 2); //UNKNOWN!
    return 0;
}
