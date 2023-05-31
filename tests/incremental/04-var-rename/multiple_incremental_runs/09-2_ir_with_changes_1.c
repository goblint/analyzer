#include <goblint.h>

void foo() {
    int fooOne = 1;
    fooOne++;
    __goblint_check(fooOne == 2);
}

void bar() {
    int barOne = 10;
    if (barOne < 11) barOne = 20;
    __goblint_check(barOne == 20);
}

int main() {
    foo();
    bar();
    return 0;
}
