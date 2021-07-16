// NOMARSHAL PARAM: --enable ana\.library --sets ana\.activated\[-\] threadid  --sets ana\.activated\[\+\] mallocWrapperTypeBased  --sets ana\.activated\[\+\] typecasts --sets ana\.activated\[-\] mallocWrapper --sets ana\.activated\[\+\] writtenLvals --sets ana\.activated\[\+\] varArgs

#include <assert.h>

int *g;

int modify(){
    int *r = g;
    int x = *g;
    *g = 43;
    *g = 12323;
    int y = *g;
}

int main()
{
    int i = 3;
    g = &i;
    modify();
    int m = *g;
    assert(i == 3); // UNKNOWN!
    return 0;
}
