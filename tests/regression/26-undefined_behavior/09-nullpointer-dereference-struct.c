// PARAM: --enable ana.nullptr --enable dbg.debug
#include <stdio.h>
struct person {
  int age;
  int height;
  int* ptr;
};

int main() {
  struct person *personPtr;
  personPtr = NULL;
  int a = (*personPtr).age; //WARN

  struct person new_person = {1,1, NULL};
  int b = *(new_person.ptr); // WARN

  int test = 3;
  struct person non_null = {1,1, &test};
  int c = *(non_null.ptr); //NOWARN
  return 0;
}
