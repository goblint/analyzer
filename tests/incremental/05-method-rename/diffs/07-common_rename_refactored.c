#include<stdio.h>

void bar() {
    printf("foo");
}

void baz() {
    printf("baz");
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
    baz();
    return 0;
}
