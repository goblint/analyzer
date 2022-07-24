// PARAM: --set ana.malloc.unique_address_count 1
#include<assert.h>
#include<stdlib.h>
int main() {
  int* arr = calloc(5,sizeof(int));
  arr[0] = 3;
  assert(arr[2] == 0); //UNKNOWN
}
