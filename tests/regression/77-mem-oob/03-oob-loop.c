// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(5 * sizeof(char));
    
    for (int i = 0; i < 10; i++) {
        ptr++;
    }

    //TODO: We cannot currently detect OOB memory accesses happening due to loops like the one above
    // => Both lines below can't have WARNs for now
    printf("%s", *ptr); //NOWARN
    free(ptr); //NOWARN

    return 0;
}
