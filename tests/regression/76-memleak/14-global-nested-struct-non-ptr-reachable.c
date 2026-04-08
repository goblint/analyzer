//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

typedef struct st {
  int *a;
  int b;
} st;

typedef struct st2 {
  st *st_ptr;
} st2;

st2 st_var;

int main(int argc, char const *argv[]) {
    st_var.st_ptr = malloc(sizeof(st));
    int *local_ptr = malloc(sizeof(int));
    st_var.st_ptr->a = local_ptr;
    local_ptr = NULL;
    free(st_var.st_ptr);

    // local_ptr's memory is reachable through st_var.st_ptr->a, but it's not freed
    // Hence, there should be only a single warning for a memory leak, but not for unreachable memory
    return 0; //WARN
}
