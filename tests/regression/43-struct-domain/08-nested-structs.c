// PARAM: --set ana.base.structs.domain "sets"

#include<assert.h>
#include<stdio.h>

struct FunctionInfo {
    void* ptr;
    int id;
};

struct Task {
    struct FunctionInfo f;
    int taskId;
    int arg;
};

struct Task task;

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

int example1() {
    int choice; // Which function to run
    int size; // Size / difficulty of algorithm

    if (size == 0) {
        if (choice == 1) {
            task.f.id = 1;
            task.f.ptr = factorial;
            task.arg = 3;
        } else {
            task.f.id = 2;
            task.f.ptr = inverseFactorial;
            task.arg = 6;
        }
        task.taskId = 0;
    } else if (size == 2) {
        if (choice == 1) {;
            task.f.id = 1;
            task.f.ptr = factorial;
            task.arg = 5;
        } else {
            task.f.id = 2;
            task.f.ptr = inverseFactorial;
            task.arg = 120;
        }
        task.taskId = 1;
    } else {
        if (choice == 1) {
            task.f.id = 1;
            task.f.ptr = factorial;
            task.arg = 10;
        } else {
            task.f.id = 2;
            task.f.ptr = inverseFactorial;
            task.arg = 3628800;
        }
        task.taskId = 2;
    }

    typedef int (*fun)(int);
    // if (task.f.id == 1) {
    //     fun f = task.f.ptr;
    //     assert(f == factorial);
    //     if (task.taskId == 0) {
    //         assert(task.arg == 3);
    //     } else if (task.taskId == 1) {
    //         assert(task.arg == 5);
    //     } else if (task.taskId == 2) {
    //         assert(task.arg == 10);
    //     }
    //     int result = f(task.arg);
    //     printf("Factorial of %d is %d\n", task.arg, result);
    // }

    if (task.f.id == 1) {
        fun f = task.f.ptr;
        assert(f == factorial);
        if (task.taskId == 0) {
            assert(task.arg == 3);
        } else if (task.taskId == 1) {
            assert(task.arg == 5);
        } else if (task.taskId == 2) {
            assert(task.arg == 10);
        }
        int result = f(task.arg);
        printf("Factorial of %d is %d\n", task.arg, result);
    } else if (task.f.id == 2) {
        fun f = task.f.ptr;
        assert(f == inverseFactorial);
        if (task.taskId == 0) {
            assert(task.arg == 6);
        } else if (task.taskId == 1) {
            assert(task.arg == 120);
        } else if (task.taskId == 2) {
            assert(task.arg == 3628800);
        }
        int result = f(task.arg);
        printf("Factorial of %d is %d\n", result, task.arg);
    } else {
        fun f = task.f.ptr;
        printf("Exiting with code %d...\n", task.arg);
        int result = f(task.arg);
    }

    return 0;
}

int example2() {
    int choice; // Which function to run
    int size; // Size / difficulty of algorithm

    if (size == 0) {
        struct FunctionInfo functionToRun;
        if (choice == 1) {
            functionToRun.id = 1;
            functionToRun.ptr = factorial;
            task.arg = 3;
        } else {
            functionToRun.id = 2;
            functionToRun.ptr = inverseFactorial;
            task.arg = 6;
        }
        // Holds two variants for functionToRun and two for task!
        task.f = functionToRun;
        // Adds the functionToRun variants to both task variants -> connection lost!
        task.taskId = 0;
    } else if (size == 2) {
        struct FunctionInfo functionToRun;
        if (choice == 1) {
            functionToRun.id = 1;
            functionToRun.ptr = factorial;
            task.arg = 5;
        } else {
            functionToRun.id = 2;
            functionToRun.ptr = inverseFactorial;
            task.arg = 120;
        }

        task.f = functionToRun;
        task.taskId = 1;
    } else {
        struct FunctionInfo functionToRun;
        if (choice == 1) {
            functionToRun.id = 1;
            functionToRun.ptr = factorial;
            task.arg = 10;
        } else {
            functionToRun.id = 2;
            functionToRun.ptr = inverseFactorial;
            task.arg = 3628800;
        }

        task.f = functionToRun;
        task.taskId = 2;
    }

    typedef int (*fun)(int);
    if (task.f.id == 1) {
        fun f = task.f.ptr;
        assert(f == factorial);
        if (task.taskId == 0) {
            assert(task.arg == 3); // UNKNOWN
        } else if (task.taskId == 1) {
            assert(task.arg == 5); // UNKNOWN
        } else if (task.taskId == 2) {
            assert(task.arg == 10); // UNKNOWN
        }
        int result = f(task.arg);
        printf("Factorial of %d is %d\n", task.arg, result);
    } else if (task.f.id == 2) {
        fun f = task.f.ptr;
        assert(f == inverseFactorial);
        if (task.taskId == 0) {
            assert(task.arg == 6); // UNKNOWN
        } else if (task.taskId == 1) {
            assert(task.arg == 120); // UNKNOWN
        } else if (task.taskId == 2) {
            assert(task.arg == 3628800); // UNKNOWN
        }
        int result = f(task.arg);
        printf("Factorial of %d is %d\n", result, task.arg);
    } else {
        fun f = task.f.ptr;
        printf("Exiting with code %d...\n", task.arg);
        int result = f(task.arg);
    }

    return 0;
}

int main() {
    example1();
    example2();
    return 0;
}
