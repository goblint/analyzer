//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] typecasts

#include <stdlib.h>
#include <assert.h>

typedef struct list {
    int val;
    struct list *next;
} list_t;

int update_list(list_t list){
    list_t *new_node = malloc(sizeof(list_t));
    new_node->val = 5;
    new_node->next = new_node;
    if(list.next != NULL) {
        list.val = 3;
        list.next->next = new_node;
    }
    return 0;
}

int main(){
    list_t node;
    list_t second;
    node.val = 1;
    node.next = &second;
    second.val = 2;
    second.next = NULL;
    update_list(node);
    assert(node.next->next == &second); //UNKNOWN
    return node.val;
}
