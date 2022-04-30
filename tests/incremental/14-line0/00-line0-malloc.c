#include<stdlib.h>
#include<stdio.h>
#include<assert.h>


#define LINEMACRO                            \
{                                                                         \
    printf("Line = %u\n", __LINE__);       \
}

int boo() {
    int x = 0;
    return 0;
}

const char* foo() {
    static size_t dfnbCapacity = 0;
    LINEMACRO
    char* dstFileNameBuffer = (char*)malloc(dfnbCapacity);
    assert(dstFileNameBuffer != 0);
    return dstFileNameBuffer;
}

int main() {
  char* p = foo();
  return 0;
}
