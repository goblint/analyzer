//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals

typedef struct list {
    int asdf;
    int val;
    struct list *next;
} list_t;

int update_list(list_t *list, int *ptr){
    *ptr = 3;
    list_t *new_node = malloc(sizeof(list_t));
    list->val = 3;
    list->next = new_node;
    return 0;
}

int main(){
    list_t node;
    list_t second;
    node.val = 4;
    node.next = &second;
    update_list(&node, &node.val);
    return node.val;
}
