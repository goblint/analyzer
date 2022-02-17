#include <stdio.h>

int main() {
    int y = 3;
    int z = 1;
    int a, b;
    int* x = &y;

    if (a) {
        if (b) {
            x = NULL;
        } else {
            x = &y;
        }
    } else {
        x = &z;
    }
    *x = 5; 
    // here Goblint says: [Error][Behavior > Undefined > NullPointerDereference][CWE-476] Must dereference NULL pointer
    return 0;
}