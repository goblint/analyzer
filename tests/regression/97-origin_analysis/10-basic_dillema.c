#include <stdio.h>
#include <stdio.h>

int main () {
    int a;
    int* b;
    int c = -2;
    int d = 3;
    int x;
    if (a) {
        b = &c;
    } else {
        b = &d;
    }
    if (a) {
        x = *b * (-1);
    } else {
        x = *b ;
    }
    assert(x < 0); //FAIL
    return 0;
}