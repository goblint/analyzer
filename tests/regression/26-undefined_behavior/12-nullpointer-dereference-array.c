// PARAM: --enable ana.nullptr --enable dbg.debug

#include <stdio.h>
// source base:
// https://stackoverflow.com/questions/4007268/what-exactly-is-meant-by-de-referencing-a-null-pointer
int main() {
  int*  arr[] = {NULL, NULL, NULL, NULL, NULL, NULL};
  int* a, b, c, x; // some integers
  int *pi;        // a pointer to an integer
  int *ok;
  a = arr[2];
  ok = &a;
  pi = &a; // pi points to a
  b = *pi; // b is now 5
  pi = NULL;
  c = *ok; // NOWARN
  c = *pi; //WARN
  int t = *arr[3];//WARN
  int* arr_2d[2][2] = {NULL, NULL, NULL, NULL};
  int m = *arr_2d[1][1]; //WARN

  return 1;
}
