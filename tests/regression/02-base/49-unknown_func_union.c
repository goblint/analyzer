#include <stdlib.h>
#include <stdio.h>


typedef struct list {
    int val;
    struct list *next;
} list_t;


typedef union either {
    int value;
    struct list node;
} either_t;

// void mutate_either(either_t e){
//     list_t *next = e.node.next;
//     next->val = 42;
// }

int main(){
    list_t first;
    list_t second;

    first.next = &second;
    first.val = 1;
    second.next = NULL;
    second.val = 2;

    either_t e;
    e.node = first;

    // When passing a union to an unknown function, reachable memory should be invalidated
    mutate_either(e);
    assert(second.val == 2); //UNKNOWN!
    return 0;
}
