// PARAM: --disable ana.base.limit-string-addresses --enable ana.int.interval --enable ana.base.arrays.nullbytes

#include <goblint.h>
#include <string.h>
#include <stdlib.h>

int main() {
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();

    return 0;
}

void example1() {
    char s1[42];
    char s2[20] = "testing"; // must null at 7, may null starting from 7

    strcpy(s1, s2); // must null and may null at 7

    size_t len = strlen(s1);
    __goblint_check(len == 7);

    strcat(s1, s2); // "testingtesting"

    len = strlen(s1);
    __goblint_check(len == 14);
}

void example2() {
    char s1[42];
    char s2[20] = "testing"; // must null at 7, may null starting from 7

    if (rand() == 42)
        s2[1] = '\0';

    strcpy(s1, s2); // may null at 1 and starting from 7

    size_t len = strlen(s1); // WARN: no must null in s1
    __goblint_check(len >= 1);
    __goblint_check(len <= 7); // UNKNOWN

    strcpy(s2, s1); // WARN: no must null in s1
}

void example3() {
    char s1[5] = "abc\0d"; // must and may null at 3
    char s2[] = "a"; // must and may null at 1

    strcpy(s1, s2); // "a\0c\0d"

    size_t len = strlen(s1);
    __goblint_check(len == 1);

    s1[1] = 'b'; // "abc\0d"
    len = strlen(s1);
    __goblint_check(len == 3);
}

void example4() {
    char s1[7] = "hello!"; // must and may null at 6
    char s2[8] = "goblint"; // must and may null at 7

    strncpy(s1, s2, 7); // WARN

    size_t len = strlen(s1); // WARN
    __goblint_check(len >= 7); // no null byte in s1
}

void example5() {
    char s1[42] = "a string, i.e. null-terminated char array"; // must and may null at 42
    for (int i = 0; i < 42; i += 3) {
        if (rand() != 42)
            s1[i] = '\0';
    }
    s1[41] = '.'; // no must nulls, only may null a 0, 3, 6...

    char s2[42] = "actually containing some text"; // must and may null at 29
    char s3[60] = "text: "; // must and may null at 6

    strcat(s3, s1); // WARN: no must nulls, may nulls at 6, 9, 12...

    size_t len = strlen(s3); // WARN
    __goblint_check(len >= 6);
    __goblint_check(len > 6); // UNKNOWN

    strncat(s2, s3, 10); // WARN: no must nulls, may nulls at 35 and 38

    len = strlen(s2); // WARN
    __goblint_check(len >= 35);
    __goblint_check(len > 40); // UNKNOWN
}

void example6() {
    char s1[50] = "hello"; // must and may null at 5
    char s2[] = " world!"; // must and may null at 7
    char s3[] = " goblint."; // must and may null at 9

    if (rand() < 42)
        strcat(s1, s2); // "hello world!" -> must and may null at 12
    else
        strncat(s1, s3, 8); // "hello goblint" -> must and may null at 13

    char s4[20];
    strcpy(s4, s1); // WARN: no must nulls, may nulls at 12 and 13

    size_t len = strlen(s4);
    __goblint_check(len >= 12);
    __goblint_check(len == 13); // UNKNOWN

    s4[14] = '\0'; // must null at 14, may nulls at 12, 13 and 14
    len = strlen(s4);
    __goblint_check(len >= 12);
    __goblint_check(len <= 14);

    char s5[20];
    strncpy(s5, s4, 16); // WARN: no must nulls, may nulls at 12, 13, 14, 15...
    len = strlen(s5); // WARN
    __goblint_check(len >= 12);
    __goblint_check(len <= 14); // UNKNOWN
    __goblint_check(len < 20); // UNKNOWN
}

void example7() {
    char s1[6] = "abc"; // must and may null at 3
    if (rand() == 42)
        s1[5] = '\0'; // must null at 3, may nulls at 3 and 5

    char s2[] = "hello world"; // must and may null at 11

    strncpy(s2, s1, 8); // WARN: 8 > size of s1 -- must and may nulls at 3, 4, 5, 6 and 7

    size_t len = strlen(s2);
    __goblint_check(len == 3);

    s2[3] = 'a'; // must and may nulls at 4, 5, 6 and 7
    len = strlen(s2);
    __goblint_check(len == 4);

    for (int i = 4; i <= 7; i++)
        s2[i] = 'a';
    s2[11] = 'a'; // no must nulls, may nulls at 4, 5, 6 and 7

    len = strlen(s2); // WARN
    __goblint_check(len >= 12); // UNKNOWN: loop transformed to interval

    s2[4] = s2[5] = s2[6] = s2[7] = 'a';
    len = strlen(s2); // WARN: no must nulls and may nulls
    __goblint_check(len >= 12);
}

void example8() {
    char empty[] = "";
    char s1[] = "hello world"; // must and may null at 11
    char s2[] = "test"; // must and may null at 4

    char cmp[50];
    strcpy(cmp, strstr(s1, empty)); // NOWARN: strstr(s1, empty) != NULL
    size_t len = strlen(cmp);
    __goblint_check(len == 11);

    char* cmp_ptr = strstr(s2, s1);
    __goblint_check(cmp_ptr == NULL);
}

void example9() {
    char empty1[] = "";
    char empty2[] = "\0 also empty";
    char s1[] = "hi";
    char s2[] = "hello";

    int i = strcmp(empty1, empty2);
    __goblint_check(i == 0);

    i = strcmp(empty1, s1);
    __goblint_check(i < 0);

    i = strcmp(s1, empty1);
    __goblint_check(i > 0);

    i = strcmp(s1, s2);
    __goblint_check(i != 0);

    i = strncmp(s1, s2, 2);
    __goblint_check(i != 0); // UNKNOWN

    s1[2] = 'a';

    i = strcmp(s1, s2); // WARN
    __goblint_check(i != 0); // UNKNOWN

    i = strncmp(s1, s2, 10); // WARN
    __goblint_check(i != 0); // UNKNOWN
}
