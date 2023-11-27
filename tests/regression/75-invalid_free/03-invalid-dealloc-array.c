#include <stdlib.h>

typedef struct custom_t {
    int x;
    int y;
} custom_t;

#define MAX_SIZE 5000

int main(int argc, char const *argv[])
{
    custom_t custom_arr[MAX_SIZE];
    free(custom_arr); //WARN

    int int_arr[MAX_SIZE];
    free(int_arr); //WARN

    char char_arr[MAX_SIZE];
    free(char_arr); //WARN

    char char_arr2[1];
    free(char_arr2); //WARN

    return 0;
}
