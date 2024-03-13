#include <stdlib.h>

char *cwe() {
    char a;
    return &a; //WARN
}

int main(int argc, char const *argv[]) {
    char *b = cwe();
    char test = *b; //WARN
    return 0;
}