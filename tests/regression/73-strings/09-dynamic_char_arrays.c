// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval --enable ana.base.arrays.nullbytes

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main () {
    example1();
    example2();
    example3();
    example4();

    return 0;
}

void example1() {
    size_t i;
    if (rand())
        i = 0;
    else
        i = 1;

    char* s1 = malloc(50);
    s1 = "goblint"; // must null at 7, may nulls starting from 7
    __goblint_check(s1[i] != '\0');

    char* s2 = malloc(6);
    s2 = "\0\0\0\0\0"; // NOWARN: all must and may nulls
    __goblint_check(s2[i] == '\0');

    strcpy(s1, s2); // must null at 0 and 7, mays nulls at 0 and starting from 7
    __goblint_check(s1[i] == '\0'); // UNKNOWN

    s1[i] = 'a'; // must null at 7, mays nulls at 0 and starting from 7

    size_t len = strlen(s1);
    __goblint_check(len >= 0);
    __goblint_check(len > 0); // UNKNOWN
    __goblint_check(len <= 7);

    s2[0] = 'a'; // all must and may null >= 1
    __goblint_check(s2[i] == '\0'); // UNKNOWN
}

void example2() {
    char* s1 = malloc(50);
    for (size_t i = 0; i < 50; i++)
        s1[i] = '\0';
    __goblint_check(s1[0] == '\0'); // UNKNOWN: no must nulls, all may nulls

    char* s2 = malloc(50);
    for (size_t i = 0; i < 50; i++)
        s2[i] = 'a';
    __goblint_check(s2[10] != '\0'); // no must and may nulls

    strcpy(s1, s2); // WARN: no must and may nulls
    strcpy(s2, "definite buffer overflow"); // WARN

    s2[49] = '\0'; // must and may null at 49

    strncpy(s1, s2, 10); // WARN
}

void example3() {
    char* s1 = malloc(10); // no must null, all may nulls
    char* s2 = malloc(10); // no must null, all may nulls
    strncpy(s1, s2, 4); // WARN: no must null, all may nulls
    __goblint_check(s1[3] == '\0'); // UNKNOWN
    
    s1[0] = 'a';
    s1[1] = 'b'; // no must null, may nulls >= 2

    strcat(s1, s2); // WARN: no must null, may nulls >= 2
    __goblint_check(s1[1] != '\0');
    __goblint_check(s1[2] == '\0'); // UNKNOWN

    int cmp = strncmp(s1, s2, 0);
    __goblint_check(cmp == 0);
}

void example4() {
    size_t size;
    if (rand())
        size = 15;
    else
        size = 20;

    char* s = malloc(size);
    
    s[17] = '\0'; // no must nulls, may null at 17
    __goblint_check(s[17] == '\0'); // UNKNOWN!
}