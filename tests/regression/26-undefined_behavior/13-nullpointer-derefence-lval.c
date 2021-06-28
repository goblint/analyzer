// PARAM: --enable ana.nullptr
#include <stdio.h>
int main() {
  int *x = NULL;
  *x = 5;
  return 1;
}
