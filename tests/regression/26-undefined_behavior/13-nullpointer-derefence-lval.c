#include <stdio.h>
int main() {
  int *x = NULL;
  *x = 5; //WARN
  return 1;
}
