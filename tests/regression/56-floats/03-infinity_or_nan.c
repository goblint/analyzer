// PARAM: --enable ana.float.interval --enable warn.float
#include <stdio.h>

void main()
{
    double data;
    
    double result1 = data + 1.0; // WARN: Could be +/-infinity or Nan
    double result2 = data / 0.; // WARN: Could be +/-infinity or Nan
}
