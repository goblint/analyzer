//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

typedef struct st {
  int *a;
  int b;
} st;

st *st_ptr;

int main(int argc, char const *argv[]) {
    st_ptr = malloc(sizeof(st));
    st_ptr->a = malloc(sizeof(int));
    st_ptr->a = NULL;
    free(st_ptr);

    // Only st_ptr->a is causing trouble here
    return 0; //WARN
}
