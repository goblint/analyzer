#include <stdio.h>

int main() {
    FILE *fp1;
    fp1 = fopen("test1.txt", "a");

    FILE *fp2;
    fp2 = fopen("test2.txt", "a");

    int b;

    if (b) {
        fclose(fp1);
    } else {
        fclose(fp2);
    }
    return 0;
}