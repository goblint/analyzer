// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
/*
    Note: the "--disable warn.info" above is a temporary workaround,
    since the GitHub CI seems to be considering Info messages as violations of NOWARN (cf. https://github.com/goblint/analyzer/issues/1151)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char const *argv[]) {
    int *ptr = malloc(4 * sizeof(int));

    // Both lines below are considered derefs => no need to warn, since ptr is pointing within its bounds
    memset(ptr, 0, 4 * sizeof(int)); //NOWARN
    printf("%p", (void *) ptr); //NOWARN
    ptr = ptr + 10; // ptr no longer points within its allocated bounds

    // Each of both lines below should now receive a WARN
    memset(ptr, 0, 4 * sizeof(int)); //WARN
    printf("%p", (void *) ptr); //WARN

    return 0;
}
