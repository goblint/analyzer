#include <stdio.h>
#include <stdio.h>

int main () {
    int a;
    int* b;
    int c = -2;
    int d = 3;
    if (a) {
        b = &c;
    } else {
        b = &d;
    }
    if (a) {
        assert(*b > 0); //FAIL
    } else {
        assert(*b < 0); //FAIL
    }
    return 0;
}