// PARAM: --set ana.activated[+] memLeak  --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail --enable verify
#include <stdlib.h>

static void *alloc(void) {
    return malloc(1);
}

int main(void) {
    free(alloc()); // Consume the unique address for the allocation site.
    void *p = alloc();
    void *q = alloc();
    // p and q point to distinct live allocations represented by the same summary address.
    if (p != q)
        return 0; // WARN
    for (;;) {}
}
