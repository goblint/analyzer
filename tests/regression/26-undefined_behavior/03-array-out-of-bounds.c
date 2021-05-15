// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"
#include <stdio.h>

int test(int b){
  return b;
}

int main()
{
  int arr[] = {1,2,3,4,5,6};      
  arr[9] = 10; //WARN
  arr[10] = 10; //WARN
    return 0;
}