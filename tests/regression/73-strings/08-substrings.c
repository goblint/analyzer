// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval --enable ana.base.arrays.nullbytes --set ana.activated[+] substr

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main() {
    example1();

    return 0;
}

void example1() {
    char s1[] = "string";
    char s2[100];
    s2[42] = '\0';

    if (rand() == 42) {
        strcpy(s2, "C-");
        strcat(s2, s1);
    } else {
        strcpy(s2, "sub");
        strncat(s2, s1, 20);
    }

    char* s3 = strstr(s2, s1);
    __goblint_check(s3 != NULL);

    size_t len = strlen(s3);
    __goblint_check(len >= 0);
    __goblint_check(len <= 42);
}
