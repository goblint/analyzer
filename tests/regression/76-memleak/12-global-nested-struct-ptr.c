//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

typedef struct st {
  int *a;
  int b;
} st;

typedef struct st2 {
  st *st_ptr;
} st2;

st2 *st_var;

int main(int argc, char const *argv[]) {
    st_var = malloc(sizeof(st2));
    st_var->st_ptr = malloc(sizeof(st));
    st_var->st_ptr->a = malloc(sizeof(int));
    st_var->st_ptr->a = NULL;
    free(st_var->st_ptr);
    free(st_var);

    // Only st_var->st_ptr->a is causing trouble here
    return 0; //WARN
}
