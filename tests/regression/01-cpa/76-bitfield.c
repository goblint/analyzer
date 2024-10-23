#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define ANY_ERROR 5  // 5
int main() {

  int testvar=11;

  int state;
  int r = rand() % 3;  // {r 7→ [0; 2],state 7→ [MIN INT; MAX INT]}
  switch (r) {
    case 0:
      state = 0; /* 0 */
      break;
    case 1:
      state = 8; /* 8 */
      break;
    default:
      state = 10; /* 10 */
      break;
  }
  // {r 7→ [0; 2],state 7→ [0; 10]}
  assert((state & ANY_ERROR) == 0);
}
