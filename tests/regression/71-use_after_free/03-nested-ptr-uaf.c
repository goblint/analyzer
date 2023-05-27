#include <stdlib.h>
#include <stdio.h>

int main() {
    int *ptr = malloc(sizeof(int));
    *ptr = 1;

    free(ptr);

    int a[2] = {0, 1};
    a[*ptr] = 5; // Should report "Use After Free (CWE-416)"

    if (a[*ptr] != 5) { // Should report "Use After Free (CWE-416)"
        free(ptr); // Should report "Double Free (CWE-415)"
    }
}