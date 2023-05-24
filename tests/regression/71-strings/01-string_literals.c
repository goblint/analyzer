// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

char* hello_world() {
    return "Hello world!";
}

void id(char* s) {
    #ifdef __APPLE__
        *0;
    #else
        strcpy(s, s); // WARN
    #endif
}

int main() {
    char* s1 = "abcde";
    char* s2 = "abcdfg";
    char* s3 = hello_world();
    char* edge_case = "hello\0world";
    
    int i = strlen(s1);
    __goblint_check(i == 5);

    i = strlen(s2);
    __goblint_check(i == 6);

    i = strlen(s3);
    __goblint_check(i == 12);

    i = strlen(edge_case);
    __goblint_check(i == 5);

    i = strcmp(s1, s2);
    __goblint_check(i < 0);

    i = strcmp(s2, "abcdfg"); // WARN
    __goblint_check(i == 0);

    char* cmp = strstr(s1, "bcd");
    i = strcmp(cmp, "bcde");
    __goblint_check(i == 0);

    cmp = strstr(s1, "bcdf");
    __goblint_check(cmp == NULL);

    if (rand() == 42)
        s3 = "hello";
    else
        s3 = "world";

    cmp = strstr(s3, "l");
    __goblint_check(cmp != NULL);

    cmp = strstr(s3, "he");
    __goblint_check(cmp != NULL); // UNKNOWN

    i = strncmp(s1, s2, 4);
    __goblint_check(i == 0);

    i = strncmp(s1, s2, 5);
    __goblint_check(i != 0);

    /* the following portion fails on macOS because of a spurious warning:
     * see issue goblint/cil#143
     *
     * remove #ifdef portions as soon as issue fixed */
    id(s2);

    #ifdef __APPLE__
        *0;
    #else
        strcpy(s1, "hi"); // WARN
    #endif

    #ifdef __APPLE__
        *0;
    #else
        strncpy(s1, "hi", 1); // WARN
    #endif

    #ifdef __APPLE__
        *0;
    #else
        strcat(s1, "hi"); // WARN
    #endif

    #ifdef __APPLE__
        *0;
    #else
        strncat(s1, "hi", 1); // WARN
    #endif
        
    #ifdef __APPLE__
        // do nothing => no warning
    #else  
        char s4[] = "hello";
        strcpy(s4, s2); // NOWARN
        strncpy(s4, s3, 2); // NOWARN

        char s5[13] = "hello";
        strcat(s5, " world"); // NOWARN
        strncat(s5, "! some further text", 1); // NOWARN
    #endif

    return 0;
}
