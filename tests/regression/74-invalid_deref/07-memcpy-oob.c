// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// TODO: The "--disable warn.info" part is a temporary fix and needs to be removed once the MacOS CI job is fixed
#include <stdlib.h>
#include <string.h>

typedef struct s {
    int a;
    char b;
} s;

int main(int argc, char const *argv[]) {
    int *a = malloc(10 * sizeof(int)); //Size is 40 bytes, assuming a 4-byte int
    int *b = malloc(15 * sizeof(int)); //Size is 60 bytes, assuming a 4-byte int

    memcpy(a, b, 40); //NOWARN
    memcpy(a, b, 10 * sizeof(int)); //NOWARN
    memcpy(a, b, 41); //WARN
    memcpy(a, b, 40000000); //WARN
    memcpy(a, b, 15 * sizeof(int)); //WARN

    int d;

    if (*argv == 42) {
        a = &d;
    } else if (*(argv + 5)) {
        int random = rand();
        a = &random;
        memcpy(a, b, 40); //WARN
    }

    memcpy(a, b, 40); //WARN
    memcpy(a, b, sizeof(a)); //WARN

    memcpy(b, a, 60); //WARN
    b += 1;
    memcpy(b, a, 60); //WARN


    s *s_ptr = malloc(sizeof(s));
    memcpy(s_ptr, a, sizeof(s)); //WARN
    memcpy(s_ptr->a, 0, sizeof(s)); //WARN
    memcpy(s_ptr->b, 0, sizeof(s)); //WARN

    memcpy(s_ptr, a, 40); //WARN
    memcpy(s_ptr, a, 60); //WARN
    memcpy(s_ptr, b, 40); //WARN
    memcpy(s_ptr, b, 60); //WARN

    s_ptr = s_ptr->b;
    memcpy(s_ptr, a, sizeof(s)); //WARN

    return 0;
}
