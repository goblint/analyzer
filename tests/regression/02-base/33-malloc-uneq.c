#include <stdlib.h>

int main(){
    int *x = malloc(sizeof(int));
    int *y = malloc(sizeof(int));

    assert(x == y); // UNKNOWN
}