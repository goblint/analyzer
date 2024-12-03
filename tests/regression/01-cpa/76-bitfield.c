//PARAM: --enable ana.int.bitfield
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define ANY_ERROR 5  // 5
int main() {
  int testvar = 235;

  int state;
  int r = rand() % 3;  // {r 7→ [0; 2],state 7→ [MIN INT; MAX INT]}
  switch (r) {
    case 0:
      state = 0; /* 0 */
      testvar = 1;
      break;
    case 1:
      state = 8; /* 8 */
      testvar = 1;
      break;
    default:
      state = 10; /* 10 */
      testvar = 1;
      break;
  }
  
  if(state & ANY_ERROR == 0) {
    printf("Error\n");
  } else {
    printf("No error\n");
  }

  // {r 7→ [0; 2],state 7→ [0; 10]}
  assert((state & ANY_ERROR) == 0);
  __goblint_check((state & ANY_ERROR) == 0);
}
