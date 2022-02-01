#include <assert.h>
#include <math.h>
#include <stdio.h>

int main (){
    double x = 4.0;

    double not_a_number = 0.0 / 0.0;
    double inifinty =  1.0 / 0.0;
    double neg_infinity = -1.0 / 0.0;

    printf("not_a_number: %lf\ninfinity: %lf\neg_infinity: %lf\n\n", not_a_number, inifinty, neg_infinity);

    double mult = x * 3.0;

    printf("x < mult: %d\n", x < mult);
    assert(x < mult);

    printf("x <= inifinty: %d\n", x <= inifinty);
    assert(x <= inifinty);

    printf("x >= neg_infinity: %d\n", x >= neg_infinity);
    assert(x >= neg_infinity);

    printf("not_a_number != not_a_number: %d\n", not_a_number != not_a_number);
    assert(not_a_number != not_a_number);

    return 0;
}
