// PARAM: --enable ana.float.interval
#include <goblint.h>

typedef union
{
    float value;
    unsigned int word;
} A;

int main()
{
    A a;
    a.word = 3212836864;
    float b = a.value;

    __goblint_check(b == -1.0f); // UNKNOWN!

    A a2;
    a2.value = -1.0f;
    unsigned int b2 = a2.word;

    __goblint_check(b2 == 1.0f); // UNKNOWN!

    int x = 100;
    float y = *(float *)(&a);
    __goblint_check(y == 100.f); // UNKNOWN!

    double i = 100.0;
    unsigned j = *(unsigned *)(&i);
    __goblint_check(j == 100); // UNKNOWN!
    return 0;
}
