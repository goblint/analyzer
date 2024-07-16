#include <stdlib.h>

typedef struct custom_t {
    char *x;
    int y;
} custom_t;

int main(int argc, char const *argv[]) {
    custom_t *struct_ptr = malloc(sizeof(custom_t));
    struct_ptr->x = malloc(10 * sizeof(char));
    realloc(&struct_ptr->x, 50); //NOWARN
    realloc(&struct_ptr->y, 50); //WARN
    realloc(struct_ptr, 2 * sizeof(custom_t)); //NOWARN
    return 0;
}
