#include <goblint.h>

int main() {
    int a, b;

    __goblint_split_begin(b);
    if (a && b) {
        return 1;
    }
    else {
        return 0;
    }
}
