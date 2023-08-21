//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stdio.h>

int main() {
    int *ptr = malloc(sizeof(int));
    *ptr = 42;

    int input1;

    if (input1) {
        free(ptr);   
    }

    *ptr = 43; //WARN
    free(ptr); //WARN

    return 0;
}