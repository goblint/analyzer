// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(5 * sizeof(char));
    
    for (int i = 0; i < 10; i++) {
        ptr++;
    }

    printf("%s", *ptr); //WARN
    free(ptr); //WARN

    return 0;
}
