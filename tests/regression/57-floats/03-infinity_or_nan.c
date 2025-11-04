// PARAM: --enable ana.float.interval
#include <float.h>

void main()
{
    double data;
    float data2;
    long double data3;

    double result1 = data + 1.0; // WARN: Could be +/-infinity or Nan
    double result2 = data / 0.;  // WARN: Could be +/-infinity or Nan

    double result3 = data2 + 1.0f; // WARN: Could be +/-infinity or Nan
    double result4 = data2 / 0.f;  // WARN: Could be +/-infinity or Nan

    double result5 = data3 + 1.0l; // WARN: Could be +/-infinity or Nan
    double result6 = data3 / 0.l;  // WARN: Could be +/-infinity or Nan

    // even though it will likely fit into a long double, the following also
    // gives a warning as our long doubles internally only have the precision of "normal" doubles
    data3 = DBL_MAX + 1.l; // WARN: Could be +/-infinity or Nan
}
