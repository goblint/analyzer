#include <stdio.h>

int* g;

int main () {
    int x =1;
    int y = 2;
    int z = 3;
    g = &x;
    int a;
    if (a) {
        g = &y;
    } else {
        g = &z;
    }
    int* p = g;
    int b = *p;
    return 0;
}