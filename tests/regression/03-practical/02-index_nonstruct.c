#include<assert.h>

typedef struct _s {
  int i;
} s;

int main(){
  int l = 17;

  s * K = malloc(sizeof(s));
  K->i = 1;
  l = K->i;
  assert(l == 17); // FAIL
  return 0;
}

/*
>>> update_offset: indexing on non-struct type (02-index-nonstruct.c:10)
>>> eval_offset: indexing on non-struct type (02-index-nonstruct.c:12)
*/
