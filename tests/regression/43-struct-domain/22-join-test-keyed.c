// PARAM: --set ana.base.structs.domain "keyed" --enable ana.int.interval

#include<assert.h>
#include<stdio.h>

struct FunctionInfo {
    int id;
    void* ptr;
};

struct FunctionInfo functionToRun;

int f1();
int f2();
int f3();
int f4();
int f5();
int f6();
int f7();

int example1() {
    int a; // Which function to run
    int b; // Size / difficulty of algorithm

    if (b == 0) {
        if (a == 1) {
            functionToRun.id = 2; // [2,2]
            functionToRun.ptr = f1;
        } else if (a == 2) {
            for (int i = 3; i < 5; i++) {
                functionToRun.id = i; // [3,4]
                functionToRun.ptr = f2;
            }
        } else if (a == 3) {
            functionToRun.id = 15; // [15,15]
            functionToRun.ptr = f3;
        } else {
            for (int i = 6; i < 8; i++) {
                functionToRun.id = i; // [6,7]
                functionToRun.ptr = f4;
            }
        }
    } else {
        if (a == 1) {
            for (int i = 2; i < 4; i++) {
                functionToRun.id = i; // [2,3]
                functionToRun.ptr = f5;
            }
        } else if (a == 2) {
            for (int i = 4; i < 7; i++) {
                functionToRun.id = i; // [4,6]
                functionToRun.ptr = f6;
            }
        } else {
            for (int i = 7; i < 11; i++) {
                functionToRun.id = i; // [7,10]
                functionToRun.ptr = f7;
            }
        }
    }

    if (functionToRun.id == 2) {
        __goblint_check(functionToRun.ptr == f1 || functionToRun.ptr == f5);
    } else if (functionToRun.id > 3 && functionToRun.id < 5) {
        __goblint_check(functionToRun.ptr != f3);
        __goblint_check(functionToRun.ptr == f2 || functionToRun.ptr == f6);
    }

    return 0;
}

int main() {
    example1();
    return 0;
}
