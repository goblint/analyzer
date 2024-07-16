#include <stdlib.h>

typedef struct custom_t {
    int x;
    int y;
} custom_t;

#define MAX_SIZE 5000

int main(int argc, char const *argv[])
{
    custom_t custom_arr[10];
    realloc(custom_arr, MAX_SIZE); //WARN

    int int_arr[100];
    realloc(int_arr, MAX_SIZE); //WARN

    char char_arr[1000];
    realloc(char_arr, MAX_SIZE); //WARN

    char char_arr2[1];
    realloc(char_arr2, MAX_SIZE); //WARN

    return 0;
}
