// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval --enable ana.base.arrays.nullbytes

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main () {
    char* s1 = malloc(50);
    s1[0] = 'a';

    char s2[50];
    s2[0] = 'a';

    int len1 = strlen(s1); //WARN
    int len2 = strlen(s2); //WARN
}
