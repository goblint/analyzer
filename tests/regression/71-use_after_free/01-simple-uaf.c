#include <stdlib.h>
#include <stdio.h>

int main() {
    int *ptr = malloc(sizeof(int));
    *ptr = 42;

    free(ptr);

    *ptr = 43; // Should report "Use After Free (CWE-416)"
    free(ptr); // Should report "Double Free (CWE-415)"
}