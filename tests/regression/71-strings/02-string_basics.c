#include <goblint.h>
#include <string.h>

void concat_1(char* s, int i) {
    if (i <= 0)
        return;
    else
        strncat(s, "10", 1);
        concat_1(s, i - 1);
}

int main() {
    char* s1 = malloc(40);
    if (!s1)
        return 1;
    strcpy(s1, "hello");

    char s2[] = " world!";
    char s3[10] = "abcd";
    char s4[20] = "abcdf";

    int i = strlen(s1);
    __goblint_check(i == 5); // UNKNOWN

    i = strlen(s2);
    __goblint_check(i == 7); // UNKNOWN

    i = strlen(s3);
    __goblint_check(i == 4); // UNKNOWN

    strcat(s1, s2);
    i = strcmp(s1, "hello world!");
    __goblint_check(i == 0); // UNKNOWN

    strcpy(s1, "hi ");
    strncpy(s1, s3, 3);
    i = strlen(s1);
    __goblint_check(i == 7); // UNKNOWN

    char* cmp = strstr(s1, " ");
    i = strcmp(cmp, s3);
    __goblint_check(i == 0); // UNKNOWN

    i = strncmp(s4, s3, 4);
    __goblint_check(i == 0); // UNKNOWN

    i = strncmp(s4, s3, 5);
    __goblint_check(i > 0); // UNKNOWN

    strncpy(s1, "", 20);
    concat_1(s1, 30);
    i = strlen(s1);
    __goblint_check(i == 30); // UNKNOWN

    cmp = strstr(s1, "0");
    __goblint_check(cmp == NULL); // UNKNOWN

    free(s1);

    return 0;
}