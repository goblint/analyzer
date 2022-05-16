#include<stdio.h>

//Changed.

void newFun(int c) {
    printf("newfun");
}

void bar1(int c) {
    if (c < 10) bar2(c + 1);
}

void bar2(int c) {
    if (c < 10) newFun(c + 1);
}

int main() {
    bar1(0);
    return 0;
}
