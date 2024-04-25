// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState
#include <goblint.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int *i = (int *)malloc(sizeof(int));
  int ***j = (int ***)malloc(sizeof(int) * 4);
  int **j2 = (int **)malloc(sizeof(int));
  int **j23 = (int **)malloc(sizeof(int));
  *j = j2;
  *(j + 3) = j23;
  int *j3 = (int *)malloc(sizeof(int));
  int *j33 = (int *)malloc(sizeof(int));
  *j2 = j3;
  **(j + 3) = j33;
  *j3 = 4;
  *j33 = 5;
  int *k = i;
  *k = 3;
  // j --> *j=j2 --> **j=j3 --> ***j=|4|
  // (j+3) --> j23 --> j33 --> |5|
  // k=i --> |3|

  // printf("***j = %d\n", ***j);             // 4
  // printf("***(j + 3) = %d\n", ***(j + 3)); // 5
  // printf("*i = %d\n", *i);                 // 3
  // printf("*k = %d\n", *k);                 // 3
  // printf("\n");

  __goblint_check(*j23 == j33);
  __goblint_check(*j2 == j3);
  __goblint_check(*i == *k);

  i = **(j + 3);

  // j --> *j=j2 --> **j=j3 --> ***j=|4|
  // (j+3) --> j23 --> j33=i --> |5|
  // k --> |3|

  // printf("***j = %d\n", ***j);             // 4
  // printf("***(j + 3) = %d\n", ***(j + 3)); // 5
  // printf("*i = %d\n", *i);                 // 5
  // printf("*k = %d\n", *k);                 // 3
  // printf("\n");

  __goblint_check(*j23 == j33);
  __goblint_check(*j2 == j3);
  __goblint_check(*i == *j33);

  *j = &k;

  // j2 --> j3 --> |4|
  // (j+3) --> j23 --> j33=i --> |5|
  // j --> *j --> k --> |3|

  // printf("***j = %d\n", ***j);             // 3
  // printf("***(j + 3) = %d\n", ***(j + 3)); // 5
  // printf("*i = %d\n", *i);                 // 5
  // printf("*k = %d\n", *k);                 // 3
  // printf("**j2 = %d\n", **j2);             // 4

  __goblint_check(*j23 == j33);
  __goblint_check(*j2 == j3);
  __goblint_check(**j == k);

  // not assignable: &k = *j;

  return 0;
}
