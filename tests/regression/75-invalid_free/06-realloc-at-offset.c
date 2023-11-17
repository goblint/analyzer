#include <stdlib.h>

#define MAX_SIZE 5000

int main(int argc, char const *argv[]) {
    char *ptr = malloc(42 * sizeof(char));
    ptr = ptr + 7;
    realloc(ptr, MAX_SIZE); //WARN

    return 0;
}
