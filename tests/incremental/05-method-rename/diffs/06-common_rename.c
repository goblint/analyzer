#include<stdio.h>

void bar() {
    printf("foo");
}

void fun1() {
    bar();
}

void fun2() {
    bar();
}

int main() {
    fun1();
    fun2();
    bar();
    return 0;
}
