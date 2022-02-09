#include <stdio.h>
#include <stdio.h>

int f (int a1, int a2) {
    int r = 1;
    int i;
    for (i = 0; i < a2; i++) {
        r *= a2;
    }
    return r;
}

int main () {
    int a, b;
    int c = 2;
    int d = 3;
    if (a) {
        b = f(c, d);
    } else {
        b = f(d, c);
    }
    int x = b;
    return 0;
}