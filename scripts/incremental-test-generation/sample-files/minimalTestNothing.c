// Run with:
// python3 RUN_CLI.py -m -dp -er -i ../sample-files/minimalTestNothing.c -rfb

#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
    int a = 10;
    int b = 20;
    
    if (a > b) {
        // Is never reached so there is registered nothing!
        printf("a is greater than b\n");
    }

    if (a < b) {
        // Is reached so there no problem
        printf("a is greater than b\n");
    }

    // Try with random inputs -> No problem
    srand(time(NULL));
    int r1 = rand();
    int r2 = rand();

    if (r1 > r2) {
        // No knowledge about outcome so no problem
        printf("a is greater than b\n");
    }

    if (r1 < r2) {
        // No knowledge about outcome so no problem
        printf("a is greater than b\n");
    }

    return 0;
}
