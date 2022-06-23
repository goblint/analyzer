// PARAM: --enable annotation.float.enabled
#include <assert.h>

int main() __attribute__((goblint_precision("no-float-interval")));
void test() __attribute__((goblint_precision("float-interval")));

int main()
{
    double a = 2.;
    assert(a == 2.); // UNKNOWN!
    test();
    return 0;
}

void test()
{
    double b = 2.;
    assert(b == 2.); // SUCCESS!
}
