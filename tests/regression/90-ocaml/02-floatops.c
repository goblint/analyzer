// PARAM: --set "ana.activated[+]" ocaml --set "mainfun[+]" "UNARY_OP" --set "mainfun[+]" "BINARY_OP" --set "mainfun[+]" "atof_double" --set "mainfun[+]" "atof_float" --set "mainfun[+]" "max_float" --set "mainfun[+]" "smallest_float" --set "mainfun[+]" "pi_float" --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Code from stubs.c in src/common/cdomains/floatOps that should be correct despite missing CAMLparam, as discussed in https://github.com/goblint/analyzer/issues/1371.

/* A small definition of the LXM state so sizeof works - from AI */
//struct LXM_state { uint64_t a; uint64_t x[2]; uint64_t s; };

/* Minimal macros to mimic expected behaviour */
#define Wsizeof(ty) ((sizeof(ty) + sizeof(value) - 1) / sizeof(value))
#define LXM_val(v) ((struct LXM_state *) Data_abstract_val(v))

#define CAMLparam1(x) __goblint_caml_param1(&x)
#define CAMLreturn(x) return (x) // From AI - CAMLreturn needs some variable named caml__frame, which is not available in our mock CAMLparam1, so we mock the return as well.


#define _GNU_SOURCE // necessary for M_PI to be defined
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <fenv.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

// Order must match with round_mode in floatOps.ml
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
        // Assert ignored to focus on the OCaml stubs.
        assert(0); // FAIL
        break;
    }
}

#define UNARY_OP(name, type, op)                                 \
    CAMLprim value name##_##type(value mode, value x)            \
    {                                                            \
        /* No need to use CAMLparam to keep mode and x as GC roots,
           because next GC poll point is at allocation in caml_copy_double.
           We have already read their values by then. */         \
        int old_roundingmode = fegetround();                     \
        change_round_mode(Int_val(mode));                        \
        volatile type r, x1 = Double_val(x);                     \
        r = op(x1);                                              \
        fesetround(old_roundingmode);                            \
        return caml_copy_double(r); /* NOWARN */                              \
        /* No need to use CAMLreturn because we don't use CAMLparam. */ \
    }

UNARY_OP(sqrt, double, sqrt);
UNARY_OP(sqrt, float, sqrtf);
UNARY_OP(acos, double, acos);
UNARY_OP(acos, float, acosf);
UNARY_OP(asin, double, asin);
UNARY_OP(asin, float, asinf);
UNARY_OP(atan, double, atan);
UNARY_OP(atan, float, atanf);
UNARY_OP(cos, double, cos);
UNARY_OP(cos, float, cosf);
UNARY_OP(sin, double, sin);
UNARY_OP(sin, float, sinf);
UNARY_OP(tan, double, tan);
UNARY_OP(tan, float, tanf);

#define BINARY_OP(name, type, op)                                \
    CAMLprim value name##_##type(value mode, value x, value y)   \
    {                                                            \
        /* No need to use CAMLparam to keep mode, x and y as GC roots,
           because next GC poll point is at allocation in caml_copy_double.
           We have already read their values by then. */         \
        int old_roundingmode = fegetround();                     \
        change_round_mode(Int_val(mode));                        \
        volatile type r, x1 = Double_val(x), y1 = Double_val(y); \
        r = x1 op y1;                                            \
        fesetround(old_roundingmode);                            \
        return caml_copy_double(r); /* NOWARN */                              \
        /* No need to use CAMLreturn because we don't use CAMLparam. */ \
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
    // No need to use CAMLparam to keep mode and str as GC roots,
    // because next GC poll point is at allocation in caml_copy_double.
    // We have already read their values by then.
    const char *s = String_val(str);
    volatile double r;
    int old_roundingmode = fegetround();
    change_round_mode(Int_val(mode));
    r = atof(s);
    fesetround(old_roundingmode);
    return caml_copy_double(r); // NOWARN
    // No need to use CAMLreturn because we don't use CAMLparam.
}

CAMLprim value atof_float(value mode, value str)
{
    // No need to use CAMLparam to keep mode and str as GC roots,
    // because next GC poll point is at allocation in caml_copy_double.
    // We have already read their values by then.
    const char *s = String_val(str); // Despite not being a value, this is a pointer into the OCaml heap and needs to be checked against GC. Doubles are also pointers.
    volatile float r;
    int old_roundingmode = fegetround(); // NOWARN
    change_round_mode(Int_val(mode)); // This makes a copy of the mode and no problems will arise.
    r = (float)atof(s);
    fesetround(old_roundingmode);
    return caml_copy_double(r); // NOWARN
    // No need to use CAMLreturn because we don't use CAMLparam.
}

// These are only given for floats as these operations involve no rounding and their OCaml implementation (Float module) can be used

CAMLprim value max_float(value unit)
{
    // No need to use CAMLparam to keep unit as GC root,
    // because we don't use it.
    return caml_copy_double(FLT_MAX); // NOWARN
    // No need to use CAMLreturn because we don't use CAMLparam.
}

CAMLprim value smallest_float(value unit)
{
    // No need to use CAMLparam to keep unit as GC root,
    // because we don't use it.
    return caml_copy_double(FLT_MIN); // NOWARN
    // No need to use CAMLreturn because we don't use CAMLparam.
}

CAMLprim value pi_float(value unit)
{
    return caml_copy_double(M_PI); // NOWARN
}
