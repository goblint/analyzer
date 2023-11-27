#include<goblint.h>
int g = 8;

int main() {
    int i;
    __atomic_store_n (&i, 12, __ATOMIC_RELAXED);
    i = __atomic_load_n (&i, __ATOMIC_RELAXED);
    __goblint_check(i == 12); //TODO

    // Should not be invalidated
    __goblint_check(g == 8);
}
