// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void functionC(int n);

void functionA(int n) {
    if (n > 0) {
        printf("Function A: %d\n", n);
        functionC(n - 1);
    }
}

void functionB(int n) {
    if (n > 0) {
        printf("Function B: %d\n", n);
        functionC(n - 1);
    }
}

void functionC(int n) {
    if (n > 0) {
        printf("Function C: %d\n", n);
        functionC(n);
    }
}

int main() {
    int n = 5;
    functionA(n + 1);
    functionB(n + 7);
    return 0;
}
