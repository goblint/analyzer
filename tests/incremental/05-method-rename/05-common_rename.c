#include<stdio.h>
// CRAM
void foo() {
    printf("foo");
}

void fun1() {
    foo();
}

void fun2() {
    foo();
}

int main() {
    fun1();
    fun2();
    foo();
    return 0;
}
