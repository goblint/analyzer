// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

typedef struct list {
    struct list* next;
    int val;
} list_t;

list_t *malloc_list_node(){
    list_t* new = malloc(sizeof(list_t));
    return new;
}

int main(){
    list_t unused;
    list_t *tmp = NULL;
    list_t *ptr = &tmp;
    list_t s = {0};
    ptr = malloc_list_node();
    list_t* a = ptr;
    a->next = &s;

    // The transfer function for this call should not overwrite the memory block we already have pointing a to
    // -- it must join new values in.
    ptr = malloc_list_node();
    list_t* b = ptr;
    b->next = a;

    // We check here that the first updates to "next" using a did not get lost after allocating b
    assert(a->next == &s); // UNKNOWN
    assert(b->next == a); //UNKNOWN

    // Check that b->next is more precise than just the "Unknown" pointer
    assert(b->next == &unused); // FAIL

    return 0;
}
