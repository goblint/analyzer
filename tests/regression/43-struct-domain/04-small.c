// PARAM: --set ana.base.structs.domain "sets"

#include <goblint.h>
#include<stdio.h>

struct FunctionInfo {
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

    if (choice == 1) {
        functionToRun.id = 1;
        functionToRun.ptr = factorial;
    } else {
        functionToRun.id = 2;
        functionToRun.ptr = inverseFactorial;
    }

    int dead = 1;
    typedef int (*fun)(int);
    if (functionToRun.id == 1) {
        fun f = functionToRun.ptr;
        __goblint_check(f == factorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", n, result);
        dead = 0;
    } else if (functionToRun.id == 2) {
        fun f = functionToRun.ptr;
        __goblint_check(f == inverseFactorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", result, n);
        dead = 0;
    } else {
        fun f = functionToRun.ptr;
        printf("Exiting with code %d...\n", n);
        int result = f(n);
    }

    __goblint_check(dead != 1);

    return 0;
}
