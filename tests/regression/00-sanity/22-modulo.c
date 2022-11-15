#include <goblint.h>
int main() {
    int x = -1;
    int m = x % 5;
    int r = x /5;
    __goblint_check(m == -1);
    __goblint_check(r == 0);

    x = 1;
    m = x%-5;
    r = x/-5;
    __goblint_check(m == 1);
    __goblint_check(r == 0);

    x = -1;
    m = x%-5;
    r = x/-5;
    __goblint_check(m == -1);
    __goblint_check(r == 0);
}
