#include<stdio.h>

void foo1() {
    printf("foo1");
}

void foo2() {
    foo1();
}

void foo3() {
    foo1();
}

int main() {
    foo2();
    foo3();
    return 0;
}
