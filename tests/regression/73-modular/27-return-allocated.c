//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

typedef struct node {
    struct node* next;
    int value;
} node_t;

int *allocate_int(){
    int* i = malloc(sizeof(int));
    return i;
}

node_t *allocate_node(){
    node_t* n = malloc(sizeof(node_t));
    return n;
}

node_t *init_node(node_t *n){
    if(n != NULL){
        n->value = 0;
        n->next = NULL;
    }
    return n;
}

node_t *add_node(node_t *n){
    if(n != NULL){
        node_t *new_node = allocate_node();
        new_node = init_node(new_node);
        n->next = new_node;
    }
}

void create_some_nodes(){
    node_t first = {0};

    __goblint_check(first.next == NULL);
    add_node(&first);
    __goblint_check(first.next != NULL); //UNKNOWN
    __goblint_check(first.next != &first);

    node_t *succ = first.next;
    __goblint_check(succ != &first);
    __goblint_check(succ == first.next); //UNKNOWN
}