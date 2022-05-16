#include<stdio.h>

void fun1() {
    printf("fun1");
}

void fun2() {
    fun1();
}

int main() {
    fun2();
    return 0;
}
