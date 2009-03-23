#include <errno.h>

int main(){
  int x = 5;
  if (errno == ENOENT) {
    x = 9;
  } 
  assert(x == 5); // UNKNOWN
  return 0;
}


/*
>>>Function definition missing for __errno_location (04-assign-to-ukwn.c:5)
>>>Assignment to unknown address (04-assign-to-ukwn.c:5)
>>>ANALYSIS IS NO LONGER SOUND! (04-assign-to-ukwn.c:5)
*/
