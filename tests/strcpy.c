#include "string.h"
#include "stdio.h"

int main(){
  char a[] = "foo";
  char b[] = "bar" ;
  char* c = "foo"; // string is constant, assignment UB
  char d[] = {"foo"};
  strcpy(a, b);
  printf("%s %s\n", a, b);
  return 0;
}
