//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>

typedef struct st {
  int *a;
  int b;
} st;

st st_nonptr;

int main(int argc, char const *argv[]) {
    st_nonptr.a = malloc(sizeof(int));
    st_nonptr.a = NULL;

    return 0; //WARN
}
