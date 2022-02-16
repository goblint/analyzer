#include <stdio.h>
#include <assert.h>

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
        assert(*b < 0);  // SUCCESS
    } else {
        assert(*b > 0);  // SUCCESS
    }
    return 0;
}