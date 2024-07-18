//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

int main(int argc, char const *argv[]) {
    int *p = malloc(sizeof(int));
    // No free => memory is leaked
    return 0; //WARN
}
