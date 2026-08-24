// PARAM: --set ana.activated[+] memLeak --set ana.malloc.unique_address_count 1
#include <stdlib.h>
#include <goblint.h>

int main() {
    int *ptr = malloc(sizeof(int));
    __goblint_check(ptr == 0); // FAIL
    free(ptr);
}