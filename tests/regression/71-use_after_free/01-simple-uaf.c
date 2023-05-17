#include <stdlib.h>
#include <stdio.h>

int main() {
    int *ptr = malloc(sizeof(int));
    *ptr = 42;

    free(ptr);

    *ptr = 43; // UAF
    free(ptr); // Double free
}