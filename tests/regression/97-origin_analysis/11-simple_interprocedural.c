#include <stdio.h>

int A;

int* f (int* p) {
    return p;
}

int main () {
    int a;
    int* x;
    if (a) {
        x = &A;
    } else {
        x = NULL;
    }
    int* y = f(x);
    assert(y != NULL); // FAIL
    return 0;
}