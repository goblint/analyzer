// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// TODO: The "--disable warn.info" part is a temporary fix and needs to be removed once the MacOS CI job is fixed
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]) {
    int *a = malloc(10 * sizeof(int)); //Size is 40 bytes, assuming a 4-byte int
    int *b = malloc(15 * sizeof(int)); //Size is 60 bytes, assuming a 4-byte int

    memset(a, 0, 40); //NOWARN
    memset(a, 0, 10 * sizeof(int)); //NOWARN
    memset(a, 0, 41); //WARN
    memset(a, 0, 40000000); //WARN

    memcpy(a, b, 40); //NOWARN
    memcpy(a, b, 10 * sizeof(int)); //NOWARN
    memcpy(a, b, 41); //WARN
    memcpy(a, b, 40000000); //WARN
    memcpy(a, b, 15 * sizeof(int)); //WARN
    return 0;
}
