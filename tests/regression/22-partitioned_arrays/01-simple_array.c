// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned
#include <assert.h>

int global;

int main(void)
{
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();
    example10();
    return 0;
}

// Simple example
void example1(void)
{
    int a[42];
    int i = 0;
    int top;

    while (i < 42) {
        a[i] = 0;
        __goblint_check(a[i] == 0);
        __goblint_check(a[0] == 0);
        __goblint_check(a[17] == 0); // UNKNOWN
        i++;
    }

    __goblint_check(a[0] == 0);
    __goblint_check(a[7] == 0);
    __goblint_check(a[41] == 0);
}

// More complicated expression to index rather than just a variable
void example2(void) {
    int a[42];
    int i = 1;

    while (i < 43) {
        a[i - 1] = 0;
        __goblint_check(a[i - 1] == 0);
        __goblint_check(a[38] == 0); // UNKNOWN
        i++;
    }

    __goblint_check(a[0] == 0);
    __goblint_check(a[7] == 0);
    __goblint_check(a[41] == 0);
}

// Two values initialized in one loop
void example3(void) {
    int a[42];
    int i = 0;

    while (i < 42) {
        a[i] = 0;
        i++;
        a[i] = 1;
        i++;
    }

    __goblint_check(a[0] == 2);   // FAIL
    __goblint_check(a[41] == 0);  // UNKNOWN
    __goblint_check(a[41] == 1);  // UNKNOWN
    __goblint_check(a[41] == -1); // FAIL
}

// Example where initialization proceeds backwards
void example4(void) {
    int a[42];
    int i = 41;

    while(i >= 12) {
        a[i] = 0;
        i--;
    }

    __goblint_check(a[i+2] == 0);
    __goblint_check(a[41] == 0);
    __goblint_check(a[i] == 0); //UNKNOWN
    __goblint_check(a[0] == 0); //UNKNOWN
}

// Example having two arrays partitioned according to one expression
void example5(void) {
    int a[42];
    int b[42];
    int i = 0;

    while(i < 42) {
        a[i] = 2;
        b[41-i] = 0;

        __goblint_check(b[7] == 0); //UNKNOWN
        __goblint_check(a[5] == 2); //UNKNOWN
        i++;
    }

    __goblint_check(a[0] == 2);
    __goblint_check(a[41] == 2);
    __goblint_check(b[0] == 0);
    __goblint_check(b[41] == 0);
}

// Example showing array becoming partitioned according to different expressions
void example6(void) {
    int a[42];
    int i = 0;
    int j = 0;
    int top;

    while(i < 42) {
        a[i] = 4;
        i++;
    }

    __goblint_check(a[17] == 4);
    __goblint_check(a[9] == 4);
    __goblint_check(a[3] == 4);
    __goblint_check(a[i-1] == 4);

    while(j<10) {
        a[j] = -1;
        j++;
    }

    __goblint_check(a[3] == -1);
    __goblint_check(a[0] == -1);
    __goblint_check(a[j-1] == -1);
    __goblint_check(a[j] == 4);
    __goblint_check(a[17] == 4);
    __goblint_check(a[j+5] == 4);
}

// This was the case where we thought we needed path-splitting
void example7(void) {
    int a[42];
    int i = 0;
    int top;

    if(top) {
        while(i < 41) {
            a[i] = 0;
            __goblint_check(a[i] == 0);
            i++;
        }
    }

    __goblint_check(a[0] == 0); // UNKNOWN
    __goblint_check(a[7] == 0); // UNKNOWN
    __goblint_check(a[41] == 0); // UNKNOWN
    __goblint_check(a[top] == 0); // UNKNOWN
}

// Check that the global variable is not used for partitioning
void example8() {
    int a[10];

    a[global] = 4;
    __goblint_check(a[global] == 4); // UNKNOWN

    for(int i=0; i <5; i++) {
        a[i] = 42;
    }

    __goblint_check(a[0] == 42);
    __goblint_check(a[1] == 42);
    __goblint_check(a[2] == 42);
    __goblint_check(a[3] == 42);
    __goblint_check(a[global] == 42);
}

// Check that arrays of types different from int are handeled correctly
void example9() {
    char a[10];
    int n;
    __goblint_check(a[3] == 800); // FAIL

    for(int i=0;i < 10; i++) {
        a[i] = 7;
    }

    __goblint_check(a[0] == 7);
    __goblint_check(a[3] == 7);

    a[3] = (char) n;
    __goblint_check(a[3] == 800); //FAIL
    __goblint_check(a[3] == 127); //UNKNOWN
    __goblint_check(a[3] == -128); //UNKNOWN
    __goblint_check(a[3] == -129); //FAIL
}

void example10() {
    int a[20];
    a[5] = 3;

    int i=5;
    a[i] = 7;
    __goblint_check(a[5] == 7);
}
