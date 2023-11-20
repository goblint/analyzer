#include <stdlib.h>

char **a;

char *cwe() {
    char *local;
    a = malloc(sizeof(char*));
    *a = local;
    return *a; //WARN
}

int main(int argc, char const *argv[]) {
    char *b = cwe();
    char test = *b; //WARN
    return 0;
}