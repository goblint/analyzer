// PARAM: --set ana.base.structs.domain "sets" --enable ana.base.structs.key.avoid-ints

#include<assert.h>
#include<stdio.h>
#include<string.h>
#include<stdlib.h>

struct FunctionInfo {
    const char *name;
    void* ptr;
    int id;
};

struct FunctionInfo functionToRun;

/// Finds the factorial of given number
int factorial(int n) {
    int acc = 1;
    for (int i = 1; i <= n; i++) {
        acc *= i;
    }
    return acc;
}

/// Finds the "n" given a "n!".
/// In case an integer "n" cannot be calculated, return the upper (ceil) number.
int inverseFactorial(int fac) {
    int product = 1;
    int n = 1;
    while (product < fac) {
        n++;
        product *= n;
    }
    printf("Inverse found!\n"); // create a side effect and prevent optimizations
    return n;
}


int main() {
    int n;
    int choice;
    printf("Write the function to execute (1 for factorial, 2 for inverse of factorial) and pass the parameter n:\n");
    scanf("%d %d", &choice, &n);

    if (choice == 1) {
        functionToRun.id = 1;
        functionToRun.name = "factorial";
        functionToRun.ptr = factorial;
    } else if (choice == 2) {
        functionToRun.id = 2;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else {
        functionToRun.id = 3;
        functionToRun.name = "outside function";
        functionToRun.ptr = exit;
    }

    for (int i = 1; i < 5; i++) {
        functionToRun.name = "test";
        functionToRun.id = i;
        functionToRun.ptr = exit;
    }

    typedef int (*fun)(int);
    if (functionToRun.id == 1) {
        fun f = functionToRun.ptr;
        assert(f == factorial || (void*)f == exit);
        assert((void*)f == exit); // TODO
        int result = f(n);
        printf("Factorial of %d is %d\n", n, result);
    } else if (functionToRun.id == 2) {
        fun f = functionToRun.ptr;
        assert(f == inverseFactorial || (void*)f == exit);
        assert((void*)f == exit); // TODO
        int result = f(n);
        printf("Factorial of %d is %d\n", result, n);
    } else {
        fun f = functionToRun.ptr;
        assert((void*)f == exit);
        printf("Exiting with code %d...\n", n);
        int result = f(n);
    }

    return 0;
}
