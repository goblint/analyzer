// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// TODO: The "--disable warn.info" part is a temporary fix and needs to be removed once the MacOS CI job is fixed
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct s {
    int a;
    char b;
} s;

int main(int argc, char const *argv[]) {
    int *a = malloc(10 * sizeof(int)); //Size is 40 bytes, assuming a 4-byte int

    memset(a, 0, 40); //NOWARN
    memset(a, 0, 10 * sizeof(int)); //NOWARN
    memset(a, 0, 41); //WARN
    memset(a, 0, 40000000); //WARN

    int d;

    if (argc == 15) {
        int c = 55;
        a = &c;
        memset(a, 0, argv[5]); //WARN
    } else if (argv[2] == 2) {
        a = &d;
    }

    memset(a, 0, 40); //WARN

    int input;
    scanf("%d", &input);
    memset(a, 0, input); //WARN



    int *b = malloc(15 * sizeof(int)); //Size is 60 bytes, assuming a 4-byte int
    memset(b, 0, 60); //NOWARN
    b += 1;
    memset(b, 0, 60); //WARN



    s *s_ptr = malloc(sizeof(s));
    memset(s_ptr, 0, sizeof(s)); //NOWARN
    memset(s_ptr->a, 0, sizeof(s)); //WARN
    memset(s_ptr->b, 0, sizeof(s)); //WARN

    s_ptr = s_ptr->a;
    memset(s_ptr, 0, sizeof(s)); //WARN
    
    return 0;
}
