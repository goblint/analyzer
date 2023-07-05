// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

char* hello_world() {
    return "Hello world!";
}

void id(char* s) {
    char* ptr = NULL; // future usage of cmp should warn: workaround for macOS test
    #ifdef __APPLE__
        #define ID int i = strcmp(ptr, "trigger warning")
    #else
        #define ID strcpy(s, s)
    #endif
    ID; // WARN
}

int main() {
    char* s1 = "abcde";
    char* s2 = "abcdfg";
    char* s3 = hello_world();
    
    size_t len = strlen(s1);
    __goblint_check(len == 5);

    len = strlen(s2);
    __goblint_check(len == 6);

    len = strlen(s3);
    __goblint_check(len == 12);

    int i = strcmp(s1, s2);
    __goblint_check(i < 0);

    i = strcmp(s2, "abcdfg");
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
     * remove #ifdef's as soon as issue fixed */
    id(s2);

    cmp = NULL; // future usage of cmp should warn: workaround for macOS test

    #ifdef __APPLE__
        #define STRCPY i = strcmp(cmp, "trigger warning")
    #else
        #define STRCPY strcpy(s1, "hi");
    #endif
    STRCPY; // WARN

    #ifdef __APPLE__
        #define STRNCPY i = strcmp(cmp, "trigger warning")
    #else
        # define STRNCPY strncpy(s1, "hi", 1)
    #endif
    STRNCPY; // WARN

    #ifdef __APPLE__
        #define STRCAT i = strcmp(cmp, "trigger warning")
    #else
        #define STRCAT strcat(s1, "hi")
    #endif
    STRCAT; // WARN

    #ifdef __APPLE__
        #define STRNCAT i = strcmp(cmp, "trigger warning")
    #else
        #define STRNCAT strncat(s1, "hi", 1)
    #endif
    STRNCAT; // WARN
        
    #ifdef __APPLE__
        // do nothing => no warning
    #else  
        char s4[] = "hello";
        strcpy(s4, s2); // NOWARN -> null byte array domain not enabled
        strncpy(s4, s3, 2); // NOWARN

        char s5[13] = "hello";
        strcat(s5, " world"); // NOWARN
        strncat(s5, "! some further text", 1); // NOWARN
    #endif

    return 0;
}
