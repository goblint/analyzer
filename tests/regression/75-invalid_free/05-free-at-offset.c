#include <stdlib.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(42 * sizeof(char));
    ptr = ptr + 7;
    free(ptr); //WARN

    return 0;
}
