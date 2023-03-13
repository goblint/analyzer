#include<stdio.h>

void foo1(int c) {
    if (c < 10) foo2(c + 1);
}

void foo2(int c) {
    if (c < 10) foo1(c + 1);
}

int main() {
    foo1(0);
    return 0;
}
