#include <goblint.h>

int main() {
    int a, b, c;

    __goblint_split_begin(b);
    if ((a || b) && c) {
        return 1;
    }
    else {
        return 0;
    }
}
