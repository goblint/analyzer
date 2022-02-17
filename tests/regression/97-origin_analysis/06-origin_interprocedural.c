#include <stdio.h>

int* f (int* a1, int* a2) {
    int sol = 1;
    int* r = &sol;
    int i;
    for (i = 0; i < *a2; i++) {
        *r *= *a2;
    }
    return &r;
}

int main () {
    int a;
    int* b;
    int c = 2;
    int d = 3;
    int* p1 = &c;
    int* p2 = &d;
    b = f(p1, p2);
    c = 1;
    if (a) {
        b = f(p1, p2);
    } else {
        b = f(p2, p1);
    }
    int* x = b;
    return 0;
}