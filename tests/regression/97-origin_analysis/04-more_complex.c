#include <stdio.h>

int main() {
    int y = 3;
    int z = 1;
    int a, b;
    int* x = &a;

    if (a) {
        if (b) {
            x = NULL;
        } else {
            x = &y;
        }
        // if we split we know the correlation of b and x
        int t  =3;
    } else {
        x = &z;
    }
    // another split because we might not even test b
    if (!b) {
        *x = 5; 
        // Goblint thinks this may be dereferencing a NULL pointer, but it's not true
        // Splitting should help 
    }
    return 0;
}