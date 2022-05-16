#include<stdio.h>

//Unchanged.

void bar1(int c) {
    if (c < 10) bar2(c + 1);
}

void bar2(int c) {
    if (c < 10) bar1(c + 1);
}

int main() {
    bar1(0);
    bar2(0);
    return 0;
}
