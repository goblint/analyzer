//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int matrix1[2][2] = {{1, 2}, {3, 4}};
    int matrix2[2][2] = {{5, 6}, {7, 8}};
    int result[2][2];

    int x11 = matrix1[0][0];
    int x12 = matrix1[0][1];
    int x21 = matrix1[1][0];
    int x22 = matrix1[1][1];

    int y11 = matrix2[0][0];
    int y12 = matrix2[0][1];
    int y21 = matrix2[1][0];
    int y22 = matrix2[1][1];

    result[0][0] = x11 * y11 + x12 * y21;
    result[0][1] = x11 * y12 + x12 * y22;
    result[1][0] = x21 * y11 + x22 * y21;
    result[1][1] = x21 * y12 + x22 * y22;

    __goblint_check(result[0][0] == 19); //SUCCESS
    __goblint_check(result[0][1] == 22); //SUCCESS
    __goblint_check(result[1][0] == 43); //SUCCESS
    __goblint_check(result[1][1] == 50); //SUCCESS

    return 0;
}

//This test case checks the correctness of matrix multiplication
