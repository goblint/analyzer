// PARAM: --enable ana.int.interval
#include "assert.h"

int main(){
    int tmp;
    unsigned char counter;
    counter = 0;
    counter = 1;
    while (counter < 10) { // (int) counter
      tmp = 1;
      /* counter = (unsigned char )((int )counter + 1); */
      counter++;
      tmp = 2;
    }
    assert(counter == 10);
    counter = (unsigned char)216;
}
