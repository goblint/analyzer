#include<stdio.h>

void bar1() {
    printf("fun1");
}

void bar2() {
    bar1();
}

int main() {
    bar2();
    return 0;
}
