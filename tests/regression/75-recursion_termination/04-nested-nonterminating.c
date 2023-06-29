// NONTERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void innerRecursiveFunction() {
    printf("Nested recursive call\n");

    // Recursive call to the innerRecursiveFunction
    innerRecursiveFunction();
}

void outerRecursiveFunction() {
    printf("Outer recursive call\n");

    // Recursive call to the outerRecursiveFunction
    outerRecursiveFunction();

    // Call to the innerRecursiveFunction
    innerRecursiveFunction();
}

int main() {
    // Call the outerRecursiveFunction
    outerRecursiveFunction();

    return 0;
}
