//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

int *g;

int main(int argc, char const *argv[]) {
    g = malloc(sizeof(int));
    // Reference to g's heap contents is lost here
    g = NULL;

    // According to `valid-memtrack`, the memory of p is unreachable and we don't have a false positive
    int *p = malloc(sizeof(int));

    return 0; //WARN
}
