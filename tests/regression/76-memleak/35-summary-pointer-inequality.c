// PARAM: --set ana.activated '["base","mallocWrapper","threadflag","memLeak"]' --set ana.base.privatization none --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail --enable exp.no-narrow --enable verify --enable ana.sv-comp.enabled --set exp.architecture 64bit --set ana.specification 'CHECK( init(main()), LTL(G valid-memcleanup) )'
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
