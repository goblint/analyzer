#include <stdatomic.h>
#include <goblint.h>

int main() {
    int i = 1;
    // sync and atomic builtins should return the correct values and update the variables:
    int res = __sync_fetch_and_add(&i, 1);
    __goblint_check(res == 1 && i == 2); // TODO
    res = __atomic_fetch_add(&i, 1, __ATOMIC_RELAXED);
    __goblint_check(res == 2 && i == 3); // TODO

    res = __sync_add_and_fetch(&i, 1);
    __goblint_check(res == 4 && i == 4); // TODO
    res = __atomic_add_fetch(&i, 1, __ATOMIC_RELAXED);
    __goblint_check(res == 5 && i == 5); // TODO

    // rapid-fire round:
    __sync_fetch_and_sub(&i, 1);
    __sync_fetch_and_or(&i, 2);
    __sync_fetch_and_and(&i, 3);
    __sync_fetch_and_xor(&i, 4);
    __sync_fetch_and_nand(&i, 5);
    __sync_sub_and_fetch(&i, 6);
    __sync_or_and_fetch(&i, 7);
    __sync_and_and_fetch(&i, 8);
    __sync_xor_and_fetch(&i, 9);
    __sync_nand_and_fetch(&i, 10);
    __goblint_check(i == -9); // TODO

    i = 5;
    __atomic_fetch_sub(&i, 1, __ATOMIC_RELAXED);
    __atomic_fetch_or(&i, 2, __ATOMIC_RELAXED);
    __atomic_fetch_and(&i, 3, __ATOMIC_RELAXED);
    __atomic_fetch_xor(&i, 4, __ATOMIC_RELAXED);
    __atomic_fetch_nand(&i, 5, __ATOMIC_RELAXED);
    __atomic_sub_fetch(&i, 6, __ATOMIC_RELAXED);
    __atomic_or_fetch(&i, 7, __ATOMIC_RELAXED);
    __atomic_and_fetch(&i, 8, __ATOMIC_RELAXED);
    __atomic_xor_fetch(&i, 9, __ATOMIC_RELAXED);
    __atomic_nand_fetch(&i, 10, __ATOMIC_RELAXED);
    __goblint_check(i == -9); // TODO

    // and also for the C11 functions:
    atomic_int j = 1;
    res = atomic_fetch_add(&j, 1);
    __goblint_check(res == 1 && j == 2); // TODO
    res = atomic_fetch_add_explicit(&j, 1, memory_order_relaxed);
    __goblint_check(res == 2 && j == 3); // TODO
    atomic_fetch_sub(&j, 1);
    atomic_fetch_sub_explicit(&j, 2, memory_order_relaxed);
    atomic_fetch_or(&j, 3);
    atomic_fetch_or_explicit(&j, 4, memory_order_relaxed);
    atomic_fetch_and(&j, 5);
    atomic_fetch_and_explicit(&j, 6, memory_order_relaxed);
    atomic_fetch_xor(&j, 7);
    atomic_fetch_xor_explicit(&j, 8, memory_order_relaxed);
    __goblint_check(j == 11); // TODO

    return 0;
}
