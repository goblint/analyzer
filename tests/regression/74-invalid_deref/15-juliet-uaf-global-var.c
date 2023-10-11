//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

int *global;

void other(void)
{
  int *data = global;
  free((void *)data);
  return;
}

int main(int argc, char **argv)
{
    int *data = (int *)malloc(400UL);
    free((void *)data);

    global = data;
    other();

    return 0;
}