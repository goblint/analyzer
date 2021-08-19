// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

typedef struct list {
    int val;
    struct list *next;
} list_t;

int update_list(list_t *list){
    list_t *new_node = malloc(sizeof(list_t));
    new_node->val = 5;
    new_node->next = new_node;
    list->val = 3;
    list->next = new_node;
    return 0;
}

int main(){
    list_t node;
    list_t second;
    node.val = 4;
    node.next = &second;
    update_list(&node);
    return node.val;
}
