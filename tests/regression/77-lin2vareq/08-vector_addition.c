//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int vector1[] = {1, 2, 3};
    int vector2[] = {4, 5, 6};
    int result[3];

    int x1 = vector1[0];
    int x2 = vector1[1];
    int x3 = vector1[2];

    int y1 = vector2[0];
    int y2 = vector2[1];
    int y3 = vector2[2];

    result[0] = x1 + y1;
    result[1] = x2 + y2;
    result[2] = x3 + y3;

    __goblint_check(result[0] == 5); //SUCCESS
    __goblint_check(result[1] == 7); //SUCCESS
    __goblint_check(result[2] == 9); //SUCCESS

    return 0;
}

//This test case checks whether the addition of corresponding elements from two vectors results in the expected values in the 'result' vector.


