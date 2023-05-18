#include <stdio.h>

int main() {
    int a = 10;
    int b = 20;

    if (a < b) {
        printf("a is less than b\n");
    }

    if (a <= b) {
        printf("a is less than or equal to b\n");
    }

    if (a > b) {
        printf("a is greater than b\n");
    }

    if (a >= b) {
        printf("a is greater than or equal to b\n");
    }

    if (a == b) {
        printf("a is equal to b\n");
    }

    if (a != b) {
        printf("a is not equal to b\n");
    }

    if (a && b) {
        printf("Both a and b are non-zero\n");
    }

    if (a || b) {
        printf("Either a or b is non-zero\n");
    }

    return 0;
}
