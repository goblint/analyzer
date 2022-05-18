#include<stdio.h>

void zap() {
    printf("drap");
}

void bar() {
    zap();
}

void foo() {
    bar();
}

int main() {
    foo();
    return 0;
}
