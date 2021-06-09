// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob --enable ana.int.interval
// Arrays within structures. Source of sample struct:
// https://codeforwin.org/2018/07/how-to-declare-initialize-and-access-array-of-structure.html
#include <stdio.h>
int main() {
  struct student {
    char name[100];
    int roll;
    float marks;
  };
  // Structure array declaration
  struct student stu[] = {
      {"vandah", 12, 89.5f},
      {"edin", 15, 98.0f},
      {"david", 17, 90.0f},
  };  
  // FIXME: this should be OK? but generates an AST error
  // stu[6].name = "thisisareallylongstringjkdsfjbkslagjbfdghfdnvbfsöajgfklgjfgjlfdsjhhgjgfbldfhghlfdgjöjsadfjhsagjflghfkdsgjhsgblkjsadgfkjabgjlsagdhlshagljhdasghsadgjh"; // WARN
  stu[0].roll = 2; //NOWARN
  stu[-1].roll = 10;    // WARN
  stu[1].marks = 90.5f; //NOWARN
  stu[20].marks = 89.5f; //WARN
  stu[2].name = "Dave"; //NOWARN
  for (int i = 0; i < 3; ++i) {
    stu[i].roll = 5; // NOWARN
  }
  for (int i = 0; i < 10; ++i) {
    stu[i].roll = 5; // WARN
  }
  return 0;
}
