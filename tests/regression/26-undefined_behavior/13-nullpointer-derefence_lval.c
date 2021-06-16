
// PARAM: --enable ana.nullptr --enable dbg.debug
#include <stdio.h>
int main(){    
  int *x = NULL;
  *x = 5;
  return 1;
  }