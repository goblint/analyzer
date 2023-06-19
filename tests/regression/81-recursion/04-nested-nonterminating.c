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
