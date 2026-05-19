#include <goblint.h>

int main() {
    int a, b;

    __goblint_split_begin(a);
    if (a && b) {
        return 1;
    }
    else {
        return 0;
    }
}
