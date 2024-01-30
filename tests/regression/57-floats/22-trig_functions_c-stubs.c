// PARAM: --enable ana.float.interval
#include <assert.h>
#include <math.h>
#include <float.h>

int main()
{
    int x;
    double s1, s2, s3, c1, c2, sc1, sc2, t1, t2;
    //s1: 0.5pi < [2.0, 7.5] < 2.5pi
    //s2: 1.5pi < [5.0, 10.5] < 3.5pi
    //s3: -0.5pi < [-1.0, -1.0] < 0.5pi
    //c1: 0pi < [0.2, 6.1] < 2pi
    //c2: pi < [3.3, 9.0] < 3pi
    //sc1: 0.5pi < [2.0, 3.0] < pi
    //sc2: 4.5pi < [14.5, 15.5] < 5pi
    //t1: 0pi-0.1 <= [-0.1, -0.1] <= 0pi+0.1
    //t2: 6pi-0.1 < [18.8, 18.9] < 6pi+0.1
    if (x) {
        s1 = 2.0;
        s2 = 5.0;
        s3 = -1.0;
        c1 = 0.2;
        c2 = 3.3;
        sc1 = 2.0;
        sc2 = 14.5;
        t1 = -0.1;
        t2 = 18.8;
    }
    else {
        s1 = 7.5;
        s2 = 10.5;
        s3 = 1.0;
        c1 = 6.1;
        c2 = 9.0;
        sc1 = 3.0;
        sc2 = 15.5;
        t1 = 0.1;
        t2 = 18.9;
    }

    //acos(x)
    assert(1.4 < acos(0.1) && acos(0.1) < 1.5); // SUCCESS

    //asin(x)
    assert(0.6 < asin(0.6) && asin(0.6) < 0.7); // SUCCESS

    //atan(x)
    assert(0.7 < atan(1.) && atan(1.) < 0.8);   // SUCCESS

    //cos(x)
    assert(-1. <= cos(c1) && cos(c1) < 0.99);   // SUCCESS
    assert(-0.99 < cos(c2) && cos(c2) <= 1.0);  // SUCCESS
    assert(-0.99 < cos(sc1) && cos(sc1) < 0.);  // SUCCESS
    assert(-0.99 < cos(sc2) && cos(sc2) < 0.);  // SUCCESS

    //sin(x)
    assert(-1. <= sin(s1) && sin(s1) < 0.99);   // SUCCESS
    assert(-0.99 < sin(s2) && sin(s2) <= 1.);   // SUCCESS
    assert(-0.99 < sin(s3) && sin(s3) <= 0.99);   // SUCCESS
    assert(0. < sin(sc1) && sin(sc1) < 0.99);   // SUCCESS
    assert(0. < sin(sc2) && sin(sc2) < 0.99);   // SUCCESS

    //tan(x)
    assert(-0.11 < tan(t1) && tan(t1) < 0.11 );   // SUCCESS
    assert(-0.1 < tan(t2) && tan(t2) < 0.1 );   // SUCCESS
}
