#include<stdlib.h>
#include<stdint.h>

void* malloc_2(size_t s) {
    return malloc(s);
}

int main() {
    int* ptr1 = malloc_2(sizeof(int));
    int* ptr2 = malloc_2(sizeof(int));

    // This is technically UB (comparing unrelated pointers)
    assert(ptr1==ptr2);

    // CIL turn this into the following, that is not UB and will fail in the concrete
    // Goblint claims that this holds
    assert((unsigned long) ptr1 == (unsigned long) ptr2); // UNKNOWN!

    // Here, we do not claim it holds, as we cast our abstract values to the type for ints on assignment
    int i1 =  (int)ptr1;
    int i2 =  (int)ptr2;
    assert(i1 == i2);

    return 0;
}
