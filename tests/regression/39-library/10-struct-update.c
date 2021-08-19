// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <stdlib.h>
#include <assert.h>

typedef struct list {
    int val;
    struct list *next;
} list_t;

int update_list(list_t list){
    int top;
    if(top) {
        list_t *new_node = malloc(sizeof(list_t));
        new_node->val = 5;
        new_node->next = new_node;
        if(list.next != NULL) {
            list.val = 3; // This update on list.val happens the *copied* struct
            list.next->next = new_node;
        }
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
