// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(5 * sizeof(char));

    ptr++;//NOWARN
    printf("%s", *ptr);//NOWARN
    ptr = ptr + 5;//NOWARN
    printf("%s", *ptr);//WARN
    *(ptr + 1) = 'b';//WARN
    *(ptr + 10) = 'c';//WARN

    free(ptr);

    return 0;
}
