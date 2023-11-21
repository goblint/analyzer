//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stdio.h>

void *my_malloc(size_t size) {
    return malloc(size);
}

void my_free(void *ptr) {
    free(ptr);
}

void *my_malloc2(size_t size) {
    return my_malloc(size);
}

void my_free2(void *ptr) {
    my_free(ptr);
}

int main(int argc, char const *argv[]) {
    char *p = my_malloc2(50 * sizeof(char));

    *(p + 42) = 'c'; //NOWARN
    printf("%s", p); //NOWARN

    my_free2(p);

    *(p + 42) = 'c'; //WARN
    // printf() is considered an implicit deref => need to warn
    printf("%s", p); //WARN

    // No dereferencing happening in the lines below => no need to warn for an invalid-deref
    // Also no need to warn for an invalid-free, as the call to free is within these functions and they're not the "free" function itself
    char *p2 = p; //NOWARN
    my_free2(p); //NOWARN
    my_free2(p2); //NOWARN

    return 0;
}
