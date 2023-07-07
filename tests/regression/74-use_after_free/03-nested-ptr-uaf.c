//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stdio.h>

int main() {
    int *ptr = malloc(sizeof(int));
    *ptr = 1;

    free(ptr);

    int a[2] = {0, 1};
    a[*ptr] = 5; //WARN

    if (a[*ptr] != 5) { //WARN
        free(ptr); //WARN
    }

    return 0;
}