//PARAM: --enable ana.int.interval

#include <limits.h>
#include <stdio.h>

int main() {
  printf("%d\n", INT_MAX + 1); //WARN
  return 0;
}