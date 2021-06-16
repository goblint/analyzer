// PARAM: --enable ana.nullptr --enable dbg.debug
#include <stdio.h>
struct person {
  int age;
  int height;
};

int main() {
  struct person *personPtr;
  personPtr = NULL;

  personPtr->age = 24;

  return 0;
}
