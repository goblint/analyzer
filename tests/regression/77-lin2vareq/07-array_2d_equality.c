//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

#include <stdio.h>

int main() {
    int matrix1[2][3] = {{1, 2, 3}, {4, 5, 6}};
    int matrix2[2][3] = {{1, 2, 3}, {4, 5, 6}};
    int matrix3[2][3] = {{7, 8, 9}, {10, 11, 12}};

    int x11 = matrix1[0][0];
    int x12 = matrix1[0][1];
    int x13 = matrix1[0][2];
    
    int x21 = matrix2[0][0];
    int x22 = matrix2[0][1];
    int x23 = matrix2[0][2];
    
    int y11 = matrix1[1][0];
    int y12 = matrix1[1][1];
    int y13 = matrix1[1][2];

    int y21 = matrix2[1][0];
    int y22 = matrix2[1][1];
    int y23 = matrix2[1][2];

    __goblint_check(x11 == x21); //SUCCESS
    __goblint_check(x12 == x22); //SUCCESS
    __goblint_check(x13 == x23); //SUCCESS
    __goblint_check(y11 == y21); //SUCCESS
    __goblint_check(y12 == y22); //SUCCESS
    __goblint_check(y13 == y23); //SUCCESS

    return 0;
}

//Individual variables are introduced to represent the elements of the matrices. The equality checks are performed on these individual variables.
