// NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void functionB(int n);
void functionC(int n);
void functionD(int n);

void functionA(int n) {
    if (n > 0) {
        printf("Function A: %d\n", n);
        functionB(n - 1);
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
        functionD(n + 1);
    }
}

void functionD(int n) {
    if (n > 0) {
        printf("Function D: %d\n", n);
        functionA(n + 1);
    }
}

int main() {
    int n = 15;
    functionA(n);
    return 0;
}
