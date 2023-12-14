//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

#define ROWS 3
#define COLS 3

int main() {
    int a[ROWS][COLS] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    int b[ROWS][COLS] = {
        {9, 8, 7},
        {6, 5, 4},
        {3, 2, 1}
    };

    int c = 2;

    int result1[ROWS][COLS];
    int result2[ROWS][COLS];

    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            result1[i][j] = c * (a[i][j] + b[i][j]);
        }
    }

    int temp1[ROWS][COLS];
    int temp2[ROWS][COLS];

    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            temp1[i][j] = c * a[i][j];
            temp2[i][j] = c * b[i][j];
        }
    }

    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            result2[i][j] = temp1[i][j] + temp2[i][j];
        }
    }

    for(int i=0; i<ROWS; i++){
        for(int j=0; j<COLS; j++){
            __goblint_check(result1[i][j] == result2[i][j]); //SUCCESS
        }
    }

    return 0;
}
