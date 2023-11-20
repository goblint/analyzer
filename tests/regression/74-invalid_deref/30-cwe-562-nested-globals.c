#include <stdlib.h>

char **a;
char *globb;

char *cwe() {
    globb = malloc(sizeof(char));
    a = malloc(sizeof(char*));
    *a = globb;
    // TODO: Not sure why we still get a warn for the line below. Need to fix it
    return *a; //NOWARN
}

int main(int argc, char const *argv[]) {
    char *b = cwe();
    char test = *b;
    return 0;
}