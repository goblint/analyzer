#include "string.h"

int main(){
  // struct s {int a; int b;};
  // struct s a[] = {1,2, 1,2};
  // // for(int i=0; i<2; i++){
  //   // a[i].a = 3;
  // // }
  // a[1].a = 3;
  int a[] = {1,2,3,4};
  int b[] = {5,6,7,8};
  memcpy(&b, &a, 4*sizeof(int));
  // int *c = b;
  // memcpy(c, &a, 4*sizeof(int));
  return 0;
}
