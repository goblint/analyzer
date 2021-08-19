// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

typedef struct list {
    int val;
    struct list *next;
} list_t;

int update_list(list_t *list, int *ptr){
    *ptr = 3;
    list_t *new_node = malloc(sizeof(list_t));
    new_node->val = 12;
    new_node->next = new_node;
    list->val = 3;
    list->next = new_node;
    return 0;
}

int main(){
    list_t some_other_list;
    list_t node;
    list_t second;
    node.val = 4;
    node.next = &second;
    second.next = NULL;
    update_list(&node, &node.val);

    // update_list(&some_other_list, &node.val);
    return node.val;
}
