// PARAM: --set ana.base.strings.domain disjoint  --enable ana.int.interval --enable ana.base.arrays.nullbytes --set ana.malloc.unique_address_count 1

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main() {
    // These should behave identically
    char s1[40];
    char* s5 = malloc(40);
    char* s6 = malloc(40);

    strcpy(s1, "hello");
    strcpy(s5, "hello");

    int len = strlen(s5);
    __goblint_check(len == 5);

    int len2 = strlen(s1);
    __goblint_check(len2 == 5);

    strcpy(s6,s5);
    int len3 = strlen(s6);
    __goblint_check(len3 == 5);

    strcpy(s5, "badabingbadaboom");
    int len2 = strlen(s5);
    __goblint_check(len2 == 16);

    return 0;
}
