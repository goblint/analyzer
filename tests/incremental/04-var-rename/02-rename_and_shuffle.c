#include<stdio.h>

//a is renamed to c, but the usage of a is replaced by b (semantic changes)
int main() {
    int a = 0;
    int b = 1;

    printf("Print %d", a);

    return 0;
}
