#include <stdio.h>

int main() {
    FILE *fp;
    int b;

    if (b) {
        fp = fopen("test1.txt", "a");
    } else {
        fp = fopen("test2.txt", "a");
    }
    fclose(fp);
    return 0;
}