
// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
#include <goblint.h>
#include <stdlib.h>

struct mystruct {
  int first;
  int second;
};

struct arrstruct {
  int first[3];
  int second[3];
};

void main(void) {
  // array of struct
  struct mystruct arrayStructs[3];

  //   printf("%d == %d \n", arrayStructs[2].first, ((int *)arrayStructs)[3]);
  __goblint_check(arrayStructs[0].first ==
                  ((int *)arrayStructs)[0]); // they are the same element
  __goblint_check(arrayStructs[1].second ==
                  ((int *)arrayStructs)[3]); // they are the same element
  __goblint_check(arrayStructs[2].first ==
                  ((int *)arrayStructs)[4]); // they are the same element

  // struct of array
  struct arrstruct structArray;
  int *pstruct = (int *)&structArray; // pointer to struct
  __goblint_check(structArray.first[0] ==
                  pstruct[0]); // they are the same element
  __goblint_check(structArray.first[2] ==
                  pstruct[2]); // they are the same element
  __goblint_check(structArray.second[0] ==
                  pstruct[3]); // they are the same element
  __goblint_check(structArray.second[2] ==
                  pstruct[5]); // they are the same element

  // array of array
  int array2D[2][2] = {{1, 2}, {3, 4}};
  __goblint_check(array2D[0][0] ==
                  *((int *)array2D + 0)); // they are the same element
  __goblint_check(array2D[1][0] ==
                  *((int *)array2D + 2)); // they are the same element
  __goblint_check(array2D[1][1] ==
                  *((int *)array2D + 3)); // they are the same element

  // arr2D[0][1] is the element and arr2D[2] is a pointer to an array
  __goblint_check(array2D[0][1] == (long)array2D[2]); // UNKNOWN!

  __goblint_check((int *)array2D[0] + 4 == (int *)array2D[2]);
  __goblint_check((int *)array2D + 4 == (int *)array2D[2]);

  __goblint_check(array2D[1][2] == *((int *)array2D + 4));
  __goblint_check((int *)array2D + 4 == (int *)array2D[2]);

  // 3D array
  int array3D[2][3][4] ;
  // = {
  //     {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}},
  //     {{13, 14, 15, 16}, {17, 18, 19, 20}, {21, 22, 23, 24}}};
  __goblint_check(array3D[1][0][3] == *((int *)array3D + 15));
  __goblint_check(array3D[1][2][0] == *((int *)array3D + 20));
  __goblint_check(array3D[1][2][3] == *((int *)array3D + 23));
  __goblint_check(array3D[0][1][1] == *((int *)array3D + 5));
}
