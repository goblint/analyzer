// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <pthread.h>
#include <assert.h>

typedef struct list {
    int value;
    struct list *next;
} list_t;


int insert(list_t *v){
    list_t *new = malloc(sizeof(list_t));
    new->next = NULL;
    new->value = 12;

    if(v == NULL)
        return 0;

    list_t *current = v;

    while(current->next != NULL){
        current = current->next;
    }
    current = new;
}

int threaded_insert(list_t *v){
    pthread_t thread;
    pthread_create(&thread, NULL, insert, v);
}

int foo(){
    list_t *first, *second;
    first = malloc(sizeof(list_t));
    second = malloc(sizeof(list_t));

    first->next = NULL;
    first->value = 1;
    second->next = NULL;
    second->value = 2;
    threaded_insert(first);

    first->next = second;

    assert(second->next == NULL);// UNKNOWN!
    return 0;
}
