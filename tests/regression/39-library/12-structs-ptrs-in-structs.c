// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <stdlib.h>
#include <assert.h>

typedef struct list {
    int val;
    struct list *next;
} list_t;

typedef struct list_container {
    list_t *first_list;
    list_t *second_list;
} list_ctr;


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

int work_on_container(list_ctr* container){
    if(container != NULL){
        append_to_list_or_cycle(container->first_list, container->second_list);
    }
}

int main(){
    list_t node3 = {3, NULL};
    list_t node2 = {2, NULL};
    list_t node1 = {1, &node2};

    list_ctr container;
    container.first_list = &node1;
    container.second_list = &node3;

    assert(container.first_list->next == &node2);
    assert(container.second_list->next == NULL);
    work_on_container(&container);
    assert(container.first_list->next == &node2); //UNKNOWN!
    assert(container.second_list->next == NULL); //UNKNOWN!

    return 0;
}

int main2(){
    list_t node3 = {3, NULL};
    list_t node2 = {2, NULL};
    list_t node1 = {1, &node2};

    list_ctr container;
    container.first_list = &node1;
    container.second_list = &node3;

    assert(container.first_list->next == &node2);
    assert(container.second_list->next == NULL);
    append_to_list_or_cycle(container.first_list, container.second_list);
    assert(container.first_list->next == &node2); //UNKNOWN!
    assert(container.second_list->next == NULL); //UNKNOWN!

    return 0;
}
