#include<stdio.h>

void newFun() {
    printf("newFun");
}

void bar1() {
    printf("foo1");
}

void foo2() {
    bar1();
}

void foo3() {
    newFun();
}

int main() {
    foo2();
    foo3();
    return 0;
}
