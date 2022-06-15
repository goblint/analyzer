// PARAM: --enable ana.float.interval --enable warn.float
#include <stdio.h>

void main()
{
    double data;
    float data2;

    double result1 = data + 1.0; // WARN: Could be +/-infinity or Nan
    double result2 = data / 0.;  // WARN: Could be +/-infinity or Nan

    double result3 = data2 + 1.0f; // WARN: Could be +/-infinity or Nan
    double result4 = data2 / 0.f;  // WARN: Could be +/-infinity or Nan
}
