// PARAM: --set ana.base.strings.domain disjoint  --enable ana.int.interval --enable ana.base.arrays.nullbytes
#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main () {
    char* s1 = malloc(50);
    s1[0] = 'a';

    char s2[50];
    s2[0] = 'a';

    // Use size_t to avoid integer warnings hiding the lack of string warnings
    size_t len1 = strlen(s1); //TODO
    size_t len2 = strlen(s2); //WARN
}
