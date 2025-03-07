#include<stdio.h>
// CRAM
void zap() {
    printf("zap");
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
