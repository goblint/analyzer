// PARAM: --set ana.base.structs.domain "sets" --enable ana.int.interval

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
    } else if (choice == 3) {
        functionToRun.id = 3;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 4) {
        functionToRun.id = 4;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 5) {
        functionToRun.id = 5;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 6) {
        functionToRun.id = 6;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 7) {
        functionToRun.id = 7;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 8) {
        functionToRun.id = 8;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 9) {
        functionToRun.id = 9;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 10) {
        functionToRun.id = 10;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 11) {
        functionToRun.id = 11;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 12) {
        functionToRun.id = 12;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 13) {
        functionToRun.id = 13;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 14) {
        functionToRun.id = 14;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 15) {
        functionToRun.id = 15;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else if (choice == 16) {
        functionToRun.id = 16;
        functionToRun.name = "inverse factorial";
        functionToRun.ptr = inverseFactorial;
    } else {
        functionToRun.id = 3;
        functionToRun.name = "outside function";
        functionToRun.ptr = exit;
    }

    typedef int (*fun)(int);
    if (functionToRun.id == 1) {
        fun f = functionToRun.ptr;
        assert(f == factorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", n, result);
    } else if (functionToRun.id == 2) {
        fun f = functionToRun.ptr;
        assert(f == inverseFactorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", result, n);
    } else {
        fun f = functionToRun.ptr;
        printf("Exiting with code %d...\n", n);
        int result = f(n);
    }

    return 0;
}
