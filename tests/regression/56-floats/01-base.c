#include <assert.h>
#include <float.h>
#include <limits.h>
#include <math.h>

int main()
{
    double middle;
    double a = DBL_MAX;
    double b = DBL_MAX;

    middle = middle * 1.; // of course all accesses before initialization should result in a warning

    middle = (a + b) / 2.; // naive way of computing the middle

    return middle - 100.; // further uses of the overflown middle value should result in a warning
}
