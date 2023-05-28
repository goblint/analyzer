#include <stdlib.h>
#include <stdio.h>

int *ptr1;

int main() {
    ptr1 = malloc(sizeof(int));
    *ptr1 = 100;

    int *ptr2 = malloc(sizeof(int));
    *ptr2 = 1;

    int *ptr3 = malloc(sizeof(int));
    *ptr3 = 10;

    free(ptr1);
    free(ptr2);

    f(ptr1, ptr2, ptr3); // Should report "Use After Free (CWE-416)" for "ptr1" and "ptr2" here

    free(ptr3); // Should report "Double Free (CWE-415)"

    return 0;
}

void f(int *p1, int *p2, int *p3) {
    *p1 = 5000; // Should report "Use After Free (CWE-416)"
    free(p1); // Should report "Double Free (CWE-415)"
    free(p2); // Should report "Double Free (CWE-415)"
    free(p3);
}