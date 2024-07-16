#include <stdlib.h>

typedef struct custom_t {
    char *x;
    int y;
} custom_t;

int main(int argc, char const *argv[]) {
    custom_t *struct_ptr = malloc(sizeof(custom_t));
    struct_ptr->x = malloc(10 * sizeof(char));
    free(&struct_ptr->x); //NOWARN
    free(&struct_ptr->y); //WARN
    free(struct_ptr); //NOWARN
    return 0;
}
