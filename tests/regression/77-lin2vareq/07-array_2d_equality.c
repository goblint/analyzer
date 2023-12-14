//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main(){
    int matrix1[2][3] = {{1, 2, 3}, {4, 5, 6}};
    int matrix2[2][3] = {{1, 2, 3}, {4, 5, 6}};
    int matrix3[2][3] = {{7, 8, 9}, {10, 11, 12}};

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 3; ++j) {
            __goblint_check(matrix1[i][j] == matrix2[i][j]); //SUCCESS
        }
    } 

    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 3; ++j) {
            __goblint_check(matrix1[i][j] == matrix3[i][j]); //FAIL
        }
    }

    return 0;

}