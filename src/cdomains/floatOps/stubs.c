#include <stdio.h>
#include <math.h>
#include <float.h>
#include <fenv.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

enum round_mode
{
    Nearest,
    ToZero,
    Up,
    Down
};

static void change_round_mode(int mode)
{
    switch (mode)
    {
    case Nearest:
        fesetround(FE_TONEAREST);
        break;
    case ToZero:
        fesetround(FE_TOWARDZERO);
        break;
    case Up:
        fesetround(FE_UPWARD);
        break;
    case Down:
        fesetround(FE_DOWNWARD);
        break;
    default:
        assert(0);
        break;
    }
}

#define BINARY_OP(name, type, op)                                \
    CAMLprim value name##_##type(value mode, value x, value y)   \
    {                                                            \
        change_round_mode(Int_val(mode));                        \
        volatile type r, x1 = Double_val(x), y1 = Double_val(y); \
        r = x1 op y1;                                            \
        change_round_mode(Nearest);                              \
        return caml_copy_double(r);                              \
    }

BINARY_OP(add, double, +);
BINARY_OP(add, float, +);
BINARY_OP(sub, double, -);
BINARY_OP(sub, float, -);
BINARY_OP(mul, double, *);
BINARY_OP(mul, float, *);
BINARY_OP(div, double, /);
BINARY_OP(div, float, /);

CAMLprim value atof_double(value mode, value str)
{
    const char *s = String_val(str);
    volatile double r;
    change_round_mode(Int_val(mode));
    r = atof(s);
    change_round_mode(Nearest);
    return caml_copy_double(r);
}

CAMLprim value atof_float(value mode, value str)
{
    const char *s = String_val(str);
    volatile float r;
    change_round_mode(Int_val(mode));
    r = (float)atof(s);
    change_round_mode(Nearest);
    return caml_copy_double(r);
}


CAMLprim value max_float(value unit)
{
    return caml_copy_double(FLT_MAX);
}

CAMLprim value smallest_float(value unit)
{
    return caml_copy_double(FLT_MIN);
}
