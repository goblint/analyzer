//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

int *g;

int main(int argc, char const *argv[]) {
    g = malloc(sizeof(int));
    // Reference to g's heap contents is lost here
    g = NULL;
    // We get a false positive for p's memory being unreachable
    // It's still leaked, but due to free() being commented out
    // TODO: Should we only improve the error reporting for unreachable memory in this case?
    int *p = malloc(sizeof(int));
    //free(p);

    return 0; //WARN
}
