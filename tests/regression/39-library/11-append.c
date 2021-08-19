// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <stdlib.h>
#include <assert.h>

typedef struct list {
    int val;
    struct list *next;
} list_t;

int append_to_list_or_cycle(list_t *list1, list_t *list2){
    int top = rand();
    if(top){
        if(list1 != NULL){
            list1->next = list1;
        }
    } else {
        list_t *current = list1;
        if(current == NULL){
            return 0;
        }
        while(current->next != NULL) {
            current = current->next;
        }
        current->next = list2;
    }
    return 0;
}

int main(){
    list_t node3 = {3, NULL};
    list_t node2 = {2, NULL};
    list_t node1 = {1, &node2};

    append_to_list_or_cycle(&node1, &node3);

    assert(node1.next == &node3); //UNKNOWN!
    assert(node1.next == &node1); //UNKNOWN!
    assert(node1.val == 1);
    return 0;
}
