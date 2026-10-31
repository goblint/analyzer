//PARAM: --enable ana.int.interval --enable sem.malloc.fail --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 1 --set ana.activated[+] useAfterFree
/*
 * Minimal, standalone MODEL of the CVE-2026-109618 Zend HashTable failure.
 * Adapted from the control-flow / initialization ordering in PHP 8.4.26:
 *   Zend/zend_hash.c: _zend_hash_str_add_or_update_i()
 *   Zend/zend_alloc.c -> zend_bailout() -> longjmp()
 *
 * This is NOT an extracted PHP source file or a full PHP exploit.
 * The `initialized` field is a ghost/invariant witness, not a PHP field.
 * PHP's real unused buckets may contain stale heap bytes, not zeroes.
 *
 * Compile: cc -std=c11 -Wall -Wextra -O0 php_oom_longjmp_slice.c -o slice
 * Run:     ./slice          # OOM: assertion failure during shutdown
 *          ./slice normal   # no OOM: successful cleanup
 */

#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <goblint.h>
typedef struct {
    char *key;
    int value;
    int initialized;  /* instrumentation for the invariant */
} Bucket;

typedef struct {
    Bucket arData[1];
    unsigned nNumUsed;
    unsigned nNumOfElements;
} HashTable;

static HashTable ht;             /* survives the nonlocal jump */
jmp_buf bailout_target;
static int force_oom;

static char *zend_string_init_model(const char *s)
{
    if (force_oom) {
        /* PHP's OOM handler eventually takes this nonlocal exit. */
        longjmp(bailout_target, 1);
    }

    size_t n = strlen(s) + 1;
    char *p = malloc(n);
    if (!p) longjmp(bailout_target, 1);
    memcpy(p, s, n);
    return p;
}

static void zend_hash_str_add_model(const char *str, int value)
{
    /* PHP's missing raw-string-key insertion publishes the bucket first. */
    unsigned idx = ht.nNumUsed++;
    ht.nNumOfElements++;
    Bucket *p = &ht.arData[idx];

    /* BUG: OOM longjmp skips initialization, but counters stay advanced. */
    p->key = zend_string_init_model(str);
    p->value = value;
    p->initialized = 1;
}

static void request_shutdown_model(void)
{
    /* Like Zend's linear destructor, trust nNumUsed to identify live entries. */
    for (unsigned i = 0; i < ht.nNumUsed; ++i) {
        Bucket *p = &ht.arData[i];
        __goblint_check(p->initialized); //UNKNOWN!
    }
}

int main(int argc, char **argv)
{
    (void)argv;
    force_oom = (argc == 1);

    if (setjmp(bailout_target) == 0) {
        zend_hash_str_add_model("field", 42);
    } else {
        puts("OOM bailout: returning to request handler");
    }

    /* A mod_php-style shutdown still runs after failed request startup. */
    request_shutdown_model();
    puts("shutdown completed");
    return 0;
}
