#include <stdlib.h>

typedef struct custom_t {
    int x;
    int y;
} custom_t;

int main(int argc, char const *argv[])
{
    custom_t *var;
    free(var); //WARN

    return 0;
}
