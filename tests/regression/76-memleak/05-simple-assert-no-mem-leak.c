//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>
#include <assert.h>

int main(int argc, char const *argv[]) {
    int *p = malloc(sizeof(int));
    assert(1);
    free(p);
    return 0; //NOWARN
}
