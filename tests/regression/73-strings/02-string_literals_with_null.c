// PARAM: --set ana.base.strings.domain disjoint --enable ana.int.interval

#include <goblint.h>
#include <string.h>

int main() {
    char* s1 = "hello\0 world\0!";
    char* s2 = "hello";
    char* s3 = "hello world!";
    char* s4 = "\0 i am the empty string";

    size_t len = strlen(s1);
    __goblint_check(len == 5);

    int i = strcmp(s1, s2);
    __goblint_check(i == 0);

    i = strcmp(s3, s1);
    __goblint_check(i > 0);

    i = strcmp(s4, "");
    __goblint_check(i == 0);

    i = strncmp(s1, s3, 5);
    __goblint_check(i == 0);

    i = strncmp(s2, s1, 7);
    __goblint_check(i == 0);

    char* cmp = strstr(s3, s1);
    i = strcmp(cmp, "hello world!");
    __goblint_check(i == 0);

    cmp = strstr(s1, "world");
    __goblint_check(cmp == NULL);

    cmp = strstr(s1, s4);
    i = strcmp(cmp, s1);
    __goblint_check(i == 0);
    i = strcmp(cmp, "hello");
    __goblint_check(i == 0);

    return 0;
}