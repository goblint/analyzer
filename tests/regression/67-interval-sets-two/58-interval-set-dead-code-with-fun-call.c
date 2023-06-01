// PARAM: --enable ana.int.interval_set
#include <goblint.h>
#include <limits.h>

int sum(int* a, int n) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += a[i];
    }
    return sum + 2 * INT_MAX;
}

int main() {
    int n;
    int a[n];
    int x;

    if (x > 1 && x < 11) {
        x -= 2;
    }

    if (x == 9) {
        x = sum(a, n);
    }

    return x == 42 ? 1 : 0;
}
