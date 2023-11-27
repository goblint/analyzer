//PARAM: --set ana.activated[+] useAfterFree
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

    // No deref happening in the function call, hence nothing to warn about
    f(ptr1, ptr2, ptr3); //NOWARN

    free(ptr3); //WARN

    return 0;
}

void f(int *p1, int *p2, int *p3) {
    *p1 = 5000; //WARN
    free(p1); //WARN
    free(p2); //WARN
    free(p3);
}