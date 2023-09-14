// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval --enable ana.base.arrays.nullbytes

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

    // Why does this not know the string length after the copy?
    // This goes into the array/array case, so it seems unrelated to blob problems.
    strcpy(s5, "badabingbadaboom");
    len2 = strlen(s5); // no must 0 bytes anywhere?

    return 0;
}
