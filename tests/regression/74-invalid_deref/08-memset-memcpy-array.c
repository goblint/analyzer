// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// TODO: The "--disable warn.info" part is a temporary fix and needs to be removed once the MacOS CI job is fixed
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]) {
    int arr[42]; // Size should be 168 bytes (with 4 byte ints)
    int *b = arr;
    

    memset(b, 0, 168); //NOWARN
    memset(b, 0, sizeof(arr)); //NOWARN
    memset(b, 0, 169); //WARN
    memset(b, 0, sizeof(arr) + 1); //WARN
    
    int *c = malloc(sizeof(arr)); // Size should be 168 bytes (with 4 byte ints)
    memcpy(b, c, 168); //NOWARN
    memcpy(b, c, sizeof(arr)); //NOWARN
    memcpy(b, c, 169); //WARN
    memcpy(b, c, sizeof(arr) + 1); //WARN

    int d;

    if (*argv == 42) {
        b = &d;
        memset(b, 0, 168); //WARN
        memcpy(b, c, 168); //WARN
    } else if (*(argv + 5)) {
        int random = rand();
        b = &random;
        memset(b, 0, 168); //WARN
        memcpy(b, c, 168); //WARN
    }

    memset(b, 0, sizeof(arr)); //WARN
    memcpy(b, c, sizeof(arr)); //WARN
    memset(b, 0, sizeof(int)); //NOWARN
    memcpy(b, c, sizeof(int)); //NOWARN
    memset(b, 0, sizeof(int) + 1); //WARN
    memcpy(b, c, sizeof(int) + 1); //WARN

    return 0;
}
