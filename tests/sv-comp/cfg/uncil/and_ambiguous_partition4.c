#include <goblint.h>

int main() {
    int a, b, c = 1;

    __goblint_split_begin(a);
    if (a && b && c) {
        return 1;
    }
    else {
        return 0;
    }
}
