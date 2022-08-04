// PARAM: --set ana.base.structs.domain "sets"

#include<assert.h>
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include "pthread.h"

struct FunctionInfo functionToRun;

struct FunctionInfo {
    const char *name;
    void* ptr;
    int id;
};

struct Args {
    int choice;
    int n;
};

pthread_t id[2];
pthread_mutex_t lock;

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

void *runCode(void *arguments) {
    struct Args *args = arguments;
    int n = args->n;
    int choice = args->choice;

    pthread_mutex_lock(&lock);
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

    typedef int (*fun)(int);
    if (functionToRun.id == 1) {
    // if (strcmp(functionToRun.name, "factorial") == 0) {
        fun f = functionToRun.ptr;
        __goblint_check(f == factorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", n, result);
    } else if (functionToRun.id == 2) {
    // } else if (strcmp(functionToRun.name, "inverse factorial") == 0) {
        fun f = functionToRun.ptr;
        __goblint_check(f == inverseFactorial);
        int result = f(n);
        printf("Factorial of %d is %d\n", result, n);
    } else {
        fun f = functionToRun.ptr;
        __goblint_check((void*)f == exit);
        printf("Exiting with code %d...\n", n);
        int result = f(n);
    }
    pthread_mutex_unlock(&lock);
    return 0;
}


int main() {
    int n;
    int choice;
    printf("Write the function to execute (1 for factorial, 2 for inverse of factorial) and pass the parameter n:\n");
    scanf("%d %d", &choice, &n);

    struct Args *args = malloc(sizeof(struct Args));
    args->n = n;
    args->choice = choice;


    pthread_create(&id[0], NULL, (void*)runCode, (void*)args);
    for (int i = 0; i < 5; i++) {
        printf("Calculating...\n");
    }

    int res;
    pthread_join(id[0], (void**)&res);

    free(args);

    return 0;
}
