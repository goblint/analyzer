//SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --set ana.relation.privatization top --enable witness.yaml.enabled
// Identical to Example 63/01; additionally checking that writing out witnesses does not crash the analyzer
#include <goblint.h>

void main(void) {
    int i;
    int j;
    int k;
    i = 2;
    j = k + 5;

    while (i < 100) {
        __goblint_check(3 * i - j + k == 1);
        i = i + 1;
        j = j + 3;
    }
    __goblint_check(3 * i - j + k == 1);
}
