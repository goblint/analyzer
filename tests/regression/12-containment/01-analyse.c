// PARAM: --analysis containment --class UES --allfuns CXX.json
/* I2 Goblint Version with debug info*/
////#line 0 "LLVM INTERNAL"
/* Provide Declarations */
#include <stdarg.h>
#include <setjmp.h>
/* get a declaration for alloca */
#if defined(__CYGWIN__) || defined(__MINGW32__)
#define  alloca(x) __builtin_alloca((x))
#define _alloca(x) __builtin_alloca((x))
#elif defined(__APPLE__)
extern void *__builtin_alloca(unsigned long);
#define alloca(x) __builtin_alloca(x)
#define longjmp _longjmp
#define setjmp _setjmp
#elif defined(__sun__)
#if defined(__sparcv9)
extern void *__builtin_alloca(unsigned long);
#else
extern void *__builtin_alloca(unsigned int);
#endif
#define alloca(x) __builtin_alloca(x)
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__arm__)
#define alloca(x) __builtin_alloca(x)
#elif defined(_MSC_VER)
#define inline _inline
#define alloca(x) _alloca(x)
#else
#include <alloca.h>
#endif

#ifndef __GNUC__  /* Can only support "linkonce" vars with GCC */
#define __attribute__(X)
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __EXTERNAL_WEAK__ __attribute__((weak_import))
#elif defined(__GNUC__)
#define __EXTERNAL_WEAK__ __attribute__((weak))
#else
#define __EXTERNAL_WEAK__
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __ATTRIBUTE_WEAK__
#elif defined(__GNUC__)
#define __ATTRIBUTE_WEAK__ __attribute__((weak))
#else
#define __ATTRIBUTE_WEAK__
#endif

#if defined(__GNUC__)
#define __HIDDEN__ __attribute__((visibility("hidden")))
#endif

#ifdef __GNUC__
#define LLVM_NAN(NanStr)   __builtin_nan(NanStr)   /* Double */
#define LLVM_NANF(NanStr)  __builtin_nanf(NanStr)  /* Float */
#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */
#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */
#define LLVM_INF           __builtin_inf()         /* Double */
#define LLVM_INFF          __builtin_inff()        /* Float */
#define LLVM_PREFETCH(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define __ATTRIBUTE_CTOR__ __attribute__((constructor))
#define __ATTRIBUTE_DTOR__ __attribute__((destructor))
#define LLVM_ASM           __asm__
#else
#define LLVM_NAN(NanStr)   ((double)0.0)           /* Double */
#define LLVM_NANF(NanStr)  0.0F                    /* Float */
#define LLVM_NANS(NanStr)  ((double)0.0)           /* Double */
#define LLVM_NANSF(NanStr) 0.0F                    /* Float */
#define LLVM_INF           ((double)0.0)           /* Double */
#define LLVM_INFF          0.0F                    /* Float */
#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */
#define __ATTRIBUTE_CTOR__
#define __ATTRIBUTE_DTOR__
#define LLVM_ASM(X)
#endif

#if __GNUC__ < 4 /* Old GCC's, or compilers not GCC */ 
#define __builtin_stack_save() 0   /* not implemented */
#define __builtin_stack_restore(X) /* noop */
#endif

#if __GNUC__ && __LP64__ /* 128-bit integer types */
typedef int llvmInt128;
typedef unsigned llvmUInt128;
#endif

#define CODE_FOR_MAIN() /* Any target-specific code for main()*/

#ifndef __cplusplus
typedef unsigned char bool;
#endif


/* Support for floating point constants */
typedef unsigned long long ConstantDoubleTy;
typedef unsigned int        ConstantFloatTy;
typedef struct { unsigned long long f1; unsigned short f2; unsigned short pad[3]; } ConstantFP80Ty;
typedef struct { unsigned long long f1; unsigned long long f2; } ConstantFP128Ty;


/* Global Declarations */
/* Helper union for bitcasts */
typedef union {
  unsigned int Int32;
  unsigned long long Int64;
  float Float;
  double Double;
} llvmBitCastUnion;
/* Structure forward decls */
struct l_class_OC_my_namespace_KD__KD_CBaseFSM;
struct l_class_OC_my_namespace_KD__KD_FSM;
struct l_class_OC_my_namespace_KD__KD_Log;
struct l_class_OC_my_namespace_KD__KD_SubUES_X;
struct l_class_OC_my_namespace_KD__KD_UES;
struct l_struct_OC_my_namespace_KD__KD_UEC_Event;
struct l_unnamed0;
struct l_unnamed1;
struct l_unnamed10;
struct l_unnamed11;
struct l_unnamed12;
struct l_unnamed13;
struct l_unnamed14;
struct l_unnamed15;
struct l_unnamed2;
struct l_unnamed3;
struct l_unnamed4;
struct l_unnamed5;
struct l_unnamed6;
struct l_unnamed7;
struct l_unnamed8;
struct l_unnamed9;

/* Typedefs */
typedef struct l_class_OC_my_namespace_KD__KD_CBaseFSM l_class_OC_my_namespace_KD__KD_CBaseFSM;
typedef struct l_class_OC_my_namespace_KD__KD_FSM l_class_OC_my_namespace_KD__KD_FSM;
typedef struct l_class_OC_my_namespace_KD__KD_Log l_class_OC_my_namespace_KD__KD_Log;
typedef struct l_class_OC_my_namespace_KD__KD_SubUES_X l_class_OC_my_namespace_KD__KD_SubUES_X;
typedef struct l_class_OC_my_namespace_KD__KD_UES l_class_OC_my_namespace_KD__KD_UES;
typedef struct l_struct_OC_my_namespace_KD__KD_UEC_Event l_struct_OC_my_namespace_KD__KD_UEC_Event;
typedef struct l_unnamed0 l_unnamed0;
typedef struct l_unnamed1 l_unnamed1;
typedef struct l_unnamed10 l_unnamed10;
typedef struct l_unnamed11 l_unnamed11;
typedef struct l_unnamed12 l_unnamed12;
typedef struct l_unnamed13 l_unnamed13;
typedef struct l_unnamed14 l_unnamed14;
typedef struct l_unnamed15 l_unnamed15;
typedef struct l_unnamed2 l_unnamed2;
typedef struct l_unnamed3 l_unnamed3;
typedef struct l_unnamed4 l_unnamed4;
typedef struct l_unnamed5 l_unnamed5;
typedef struct l_unnamed6 l_unnamed6;
typedef struct l_unnamed7 l_unnamed7;
typedef struct l_unnamed8 l_unnamed8;
typedef struct l_unnamed9 l_unnamed9;

/* Structure contents */
struct l_class_OC_my_namespace_KD__KD_CBaseFSM {  unsigned int field0;};

struct l_unnamed12 { unsigned char array[4]; };

struct l_unnamed15 { float array[4]; };

struct l_class_OC_my_namespace_KD__KD_FSM {  unsigned int  (**field0) ( int, ...);  struct l_unnamed12 field1;  struct l_unnamed15 field2;};

struct l_unnamed8 { unsigned int array[10]; };

struct l_class_OC_my_namespace_KD__KD_Log {  struct l_unnamed12 field0;  unsigned int field1;  struct l_unnamed8 field2;  struct l_unnamed8 field3;  unsigned int field4;};

struct l_unnamed1 { unsigned char array[28]; };

struct l_unnamed4 { unsigned char array[92]; };

struct l_class_OC_my_namespace_KD__KD_SubUES_X {  struct l_unnamed1 field0;  struct l_unnamed4 field1;  unsigned int field2;  struct l_unnamed12 field3;};

struct l_class_OC_my_namespace_KD__KD_UES {  struct l_unnamed1 field0;  struct l_unnamed4 field1;  unsigned int field2;  struct l_unnamed12 field3;  struct l_class_OC_my_namespace_KD__KD_SubUES_X field4;  unsigned int *field5;};

struct l_struct_OC_my_namespace_KD__KD_UEC_Event {  unsigned int field0;  unsigned int field1;};

struct l_unnamed0 { unsigned char array[23]; };

struct l_unnamed10 { unsigned char array[26]; };

struct l_unnamed11 { unsigned char *array[5]; };

struct l_unnamed13 { unsigned char array[21]; };

struct l_unnamed14 {  unsigned long long field0;  unsigned long long field1;};

struct l_unnamed2 { unsigned char *array[3]; };

struct l_unnamed3 { unsigned char array[5]; };

struct l_unnamed5 {  unsigned char *field0;  unsigned char *field1;};

struct l_unnamed6 { unsigned char array[15]; };

struct l_unnamed7 {  unsigned char *field0;  unsigned char *field1;  unsigned int field2;  unsigned int field3;  unsigned char *field4;  unsigned long long field5;  unsigned char *field6;  unsigned long long field7;};

struct l_unnamed9 {  unsigned char *field0;  unsigned char *field1;  unsigned int field2;  unsigned int field3;  unsigned char *field4;  unsigned long long field5;};


/* External Global Variable Declarations */
extern unsigned int API_0;
extern unsigned int API_1;
extern unsigned char *_ZTVN10__cxxabiv121__vmi_class_type_infoE;
extern unsigned char *_ZTVN10__cxxabiv117__class_type_infoE;
extern unsigned int _ZN12my_namespace8CBaseFSM5fubarE;

/* Function Declarations */
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
void _Z5__GWNv(void);
void _Z5__GERv(void);
void _Z5__GNWv(void);
void _ZN12my_namespace5dummyEv(void);
void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int __gxx_personality_v0(int vararg_dummy_arg,...);
void _ZSt9terminatev(void);
void _Unwind_Resume_or_Rethrow(unsigned char *);
void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD0Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZdlPv(unsigned char *);
unsigned char *_ZN12my_namespace3UES13PrivateMemberEPii(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y) __ATTRIBUTE_WEAK__;
unsigned char *_ZN12my_namespace3UES14PrivateMember2EPii(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3Log5doLogEPc(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned char *llvm_cbe_txt) __ATTRIBUTE_WEAK__;
unsigned int _Z9API_CALL0i(unsigned int );
unsigned int printf(unsigned char *,...);
unsigned int _Z9API_CALL1iPv(unsigned int , unsigned char *);
void _ZN12my_namespace8SubUES_XD2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD0Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_unnamed14 _ZN12my_namespace3Log13get_prifv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3Log11my_priv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESC2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3FSMC2Ev(struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XC1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XC2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8CBaseFSMC2Ev(struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void __cxa_pure_virtual(void);
void abort(void);


/* Global Variable Declarations */
extern unsigned int API_0;
extern unsigned int API_1;
extern struct l_unnamed11 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed5 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed0 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed5 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed7 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__;
static struct l_unnamed6 _OC_str;
extern struct l_unnamed11 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed7 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed2 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__;
static struct l_unnamed3 _OC_str1;


/* Global Variable Definitions and Initialization */
unsigned int API_0;
unsigned int API_1;
struct l_unnamed11 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3UESE)), ((unsigned char *)_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace3UESD1Ev), ((unsigned char *)_ZN12my_namespace3UESD0Ev) } };
struct l_unnamed13 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { "N12my_namespace3UESE" };
struct l_unnamed13 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { "N12my_namespace3FSME" };
struct l_unnamed10 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { "N12my_namespace8CBaseFSME" };
struct l_unnamed5 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8CBaseFSME.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3FSME.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace8CBaseFSME)), 2050ull };
struct l_unnamed13 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { "N12my_namespace3LogE" };
struct l_unnamed0 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { "N12my_namespace5Test2E" };
struct l_unnamed5 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace5Test2E.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3LogE.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace5Test2E)), 0ull };
struct l_unnamed7 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3UESE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 7170ull };
static struct l_unnamed6 _OC_str = { "received event" };
struct l_unnamed11 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace8SubUES_XE)), ((unsigned char *)_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace8SubUES_XD1Ev), ((unsigned char *)_ZN12my_namespace8SubUES_XD0Ev) } };
struct l_unnamed10 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { "N12my_namespace8SubUES_XE" };
struct l_unnamed7 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8SubUES_XE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 7170ull };
struct l_unnamed2 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3FSME)), ((unsigned char *)__cxa_pure_virtual) } };
static struct l_unnamed3 _OC_str1 = { "test" };


/* Function Bodies */
static inline int llvm_fcmp_ord(double X, double Y) { return X == X && Y == Y; }
static inline int llvm_fcmp_uno(double X, double Y) { return X != X || Y != Y; }
static inline int llvm_fcmp_ueq(double X, double Y) { return X == Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_une(double X, double Y) { return X != Y; }
static inline int llvm_fcmp_ult(double X, double Y) { return X <  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ugt(double X, double Y) { return X >  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ule(double X, double Y) { return X <= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_uge(double X, double Y) { return X >= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_oeq(double X, double Y) { return X == Y ; }
static inline int llvm_fcmp_one(double X, double Y) { return X != Y && llvm_fcmp_ord(X, Y); }
static inline int llvm_fcmp_olt(double X, double Y) { return X <  Y ; }
static inline int llvm_fcmp_ogt(double X, double Y) { return X >  Y ; }
static inline int llvm_fcmp_ole(double X, double Y) { return X <= Y ; }
static inline int llvm_fcmp_oge(double X, double Y) { return X >= Y ; }

////#line 0 "LLVM INTERNAL"
void _Z5__GWNv(void) {
////#line 4 "regression.h"
  return;
}


////#line 0 "LLVM INTERNAL"
void _Z5__GERv(void) {
////#line 5 "regression.h"
  return;
}


////#line 0 "LLVM INTERNAL"
void _Z5__GNWv(void) {
////#line 6 "regression.h"
  return;
}


void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this);

void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob);

////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace5dummyEv(void) {
  struct l_class_OC_my_namespace_KD__KD_UES llvm_cbe_ues;    /* Address-exposed local */
  unsigned int llvm_cbe_i;    /* Address-exposed local */

////#line 222 "small.cpp"
  _ZN12my_namespace3UESC1Ev((&llvm_cbe_ues));
////#line 224 "small.cpp"
  _ZN12my_namespace3UES16InsertGlobalDataEPi((&llvm_cbe_ues), (&llvm_cbe_i));
////#line 225 "small.cpp"
  _ZN12my_namespace3UESD1Ev((&llvm_cbe_ues));
////#line 225 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESC2Ev(llvm_cbe_this1);
////#line 135 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESD2Ev(llvm_cbe_this1);
////#line 139 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pglob_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pglob_2e_addr) = llvm_cbe_pglob;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 170 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pglob_2e_addr);
////#line 170 "small.cpp"
  *((&llvm_cbe_this1->field5)) = llvm_cbe_tmp;
////#line 171 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3UESE.array[((signed long long )2ull)]));
////#line 139 "small.cpp"
  _ZN12my_namespace8SubUES_XD1Ev(((&llvm_cbe_this1->field4)));
////#line 139 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XD2Ev(llvm_cbe_this1);
////#line 110 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_mi;    /* Address-exposed local */
  unsigned int *llvm_cbe_psx;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned char *llvm_cbe_call;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp5;
  unsigned char *llvm_cbe_call6;
  unsigned int *llvm_cbe_tmp8;
  unsigned int llvm_cbe_tmp9;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp10;
  unsigned int *llvm_cbe_tmp12;
  unsigned int llvm_cbe_tmp13;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp15;
  unsigned int llvm_cbe_tmp17;
  unsigned int llvm_cbe_tmp20;
  unsigned int llvm_cbe_call21;
  unsigned int llvm_cbe_tmp23;
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_tmp27;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM * (**llvm_cbe_tmp__1) (struct l_class_OC_my_namespace_KD__KD_SubUES_X *, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *);
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM * (*llvm_cbe_tmp__2) (struct l_class_OC_my_namespace_KD__KD_SubUES_X *, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *);
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp28;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call29;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp30;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp32;
  unsigned int llvm_cbe_tmp34;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp37;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp39;
  unsigned int llvm_cbe_tmp41;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp44;
  unsigned int llvm_cbe_tmp46;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp50;
  unsigned int llvm_cbe_tmp54;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp57;
  unsigned int llvm_cbe_tmp59;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp63;
  unsigned int llvm_cbe_tmp65;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__3;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 175 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field5));
////#line 175 "small.cpp"
  llvm_cbe_tmp3 = *((&(*llvm_cbe_tmp2)));
////#line 175 "small.cpp"
  *(&llvm_cbe_mi) = llvm_cbe_tmp3;
////#line 175 "small.cpp"
   // WARN;
////#line 177 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3UES13PrivateMemberEPii(llvm_cbe_this1, ((&llvm_cbe_this1->field2)), 0u);
////#line 177 "small.cpp"
   // WARN;
////#line 178 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_ev_2e_addr);
////#line 178 "small.cpp"
  llvm_cbe_call6 = _ZN12my_namespace3UES14PrivateMember2EPii(llvm_cbe_this1, (((unsigned int *)llvm_cbe_tmp5)), 0u);
////#line 178 "small.cpp"
   // WARN;
////#line 180 "small.cpp"
  _ZN12my_namespace3Log5doLogEPc((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])))), ((&_OC_str.array[((signed int )0u)])));
////#line 180 "small.cpp"
   // WARN;
////#line 181 "small.cpp"
  llvm_cbe_tmp8 = (&(((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)]))))->field1);
////#line 181 "small.cpp"
  llvm_cbe_tmp9 = *llvm_cbe_tmp8;
////#line 181 "small.cpp"
  *llvm_cbe_tmp8 = (((unsigned int )(((unsigned int )llvm_cbe_tmp9) + ((unsigned int )1u))));
////#line 181 "small.cpp"
   // WARN;
////#line 183 "small.cpp"
  llvm_cbe_tmp10 = *(&llvm_cbe_ev_2e_addr);
////#line 183 "small.cpp"
  *((&llvm_cbe_tmp10->field1)) = 0u;
////#line 183 "small.cpp"
   // WARN;
////#line 185 "small.cpp"
  llvm_cbe_tmp12 = (&llvm_cbe_this1->field2);
////#line 185 "small.cpp"
  llvm_cbe_tmp13 = *llvm_cbe_tmp12;
////#line 185 "small.cpp"
  *llvm_cbe_tmp12 = (((unsigned int )(((unsigned int )llvm_cbe_tmp13) + ((unsigned int )1u))));
////#line 185 "small.cpp"
   // WARN;
////#line 186 "small.cpp"
  llvm_cbe_tmp15 = *(&llvm_cbe_ev_2e_addr);
////#line 186 "small.cpp"
  llvm_cbe_tmp17 = *((&llvm_cbe_tmp15->field1));
////#line 186 "small.cpp"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp17;
////#line 186 "small.cpp"
   // WARN;
////#line 188 "small.cpp"
  llvm_cbe_tmp20 = *((&llvm_cbe_this1->field2));
////#line 188 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp20) % ((signed int )2u)))) == 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
////#line 189 "small.cpp"
  llvm_cbe_call21 = _Z9API_CALL0i(0u);
////#line 189 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
////#line 189 "small.cpp"
   // WARN;
////#line 190 "small.cpp"
  llvm_cbe_tmp23 = *((&llvm_cbe_this1->field2));
////#line 190 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp23) % ((signed int )2u)))) != 0u)) {    goto llvm_cbe_if_2e_then26;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then26:
////#line 192 "small.cpp"
  llvm_cbe_tmp27 = (&llvm_cbe_this1->field4);
////#line 192 "small.cpp"
  llvm_cbe_tmp__1 = *(((struct l_class_OC_my_namespace_KD__KD_CBaseFSM * (***) (struct l_class_OC_my_namespace_KD__KD_SubUES_X *, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *))llvm_cbe_tmp27));
////#line 192 "small.cpp"
  llvm_cbe_tmp__2 = *((&(*llvm_cbe_tmp__1)));
////#line 192 "small.cpp"
  llvm_cbe_tmp28 = *(&llvm_cbe_ev_2e_addr);
////#line 192 "small.cpp"
  llvm_cbe_call29 = llvm_cbe_tmp__2(llvm_cbe_tmp27, llvm_cbe_tmp28);
////#line 192 "small.cpp"
   // WARN;
////#line 193 "small.cpp"
  goto llvm_cbe_if_2e_end31;

llvm_cbe_if_2e_else:
////#line 195 "small.cpp"
  llvm_cbe_tmp30 = *(&llvm_cbe_ev_2e_addr);
////#line 195 "small.cpp"
  if ((llvm_cbe_tmp30 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end31;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
////#line 195 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp30)));
////#line 195 "small.cpp"
  goto llvm_cbe_if_2e_end31;

llvm_cbe_if_2e_end31:
////#line 197 "small.cpp"
  llvm_cbe_tmp32 = *(&llvm_cbe_ev_2e_addr);
////#line 197 "small.cpp"
  llvm_cbe_tmp34 = *((&llvm_cbe_tmp32->field1));
////#line 197 "small.cpp"
  if ((llvm_cbe_tmp34 == 1u)) {    goto llvm_cbe_if_2e_then36;  } else {    goto llvm_cbe_if_2e_end38;  }


llvm_cbe_if_2e_then36:
////#line 198 "small.cpp"
  llvm_cbe_tmp37 = *(&llvm_cbe_ev_2e_addr);
////#line 198 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_tmp37));
////#line 198 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end38:
////#line 198 "small.cpp"
   // WARN;
////#line 200 "small.cpp"
  llvm_cbe_tmp39 = *(&llvm_cbe_ev_2e_addr);
////#line 200 "small.cpp"
  llvm_cbe_tmp41 = *((&llvm_cbe_tmp39->field1));
////#line 200 "small.cpp"
  if ((llvm_cbe_tmp41 == 2u)) {    goto llvm_cbe_if_2e_then43;  } else {    goto llvm_cbe_if_2e_end47;  }


llvm_cbe_if_2e_then43:
////#line 201 "small.cpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_ev_2e_addr);
////#line 201 "small.cpp"
  llvm_cbe_tmp46 = *((&llvm_cbe_tmp44->field1));
////#line 201 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((unsigned long long )(unsigned int )llvm_cbe_tmp46))));
////#line 201 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end47:
////#line 201 "small.cpp"
   // WARN;
////#line 203 "small.cpp"
  llvm_cbe_tmp48 = *(&llvm_cbe_ev_2e_addr);
////#line 203 "small.cpp"
  llvm_cbe_tmp50 = *((&llvm_cbe_tmp48->field1));
////#line 203 "small.cpp"
  if ((llvm_cbe_tmp50 == 3u)) {    goto llvm_cbe_if_2e_then52;  } else {    goto llvm_cbe_if_2e_end56;  }


llvm_cbe_if_2e_then52:
////#line 204 "small.cpp"
  llvm_cbe_tmp54 = *((&llvm_cbe_this1->field2));
////#line 204 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((signed long long )(signed int )llvm_cbe_tmp54))));
////#line 204 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end56:
////#line 204 "small.cpp"
   // WARN;
////#line 206 "small.cpp"
  llvm_cbe_tmp57 = *(&llvm_cbe_ev_2e_addr);
////#line 206 "small.cpp"
  llvm_cbe_tmp59 = *((&llvm_cbe_tmp57->field1));
////#line 206 "small.cpp"
  if ((llvm_cbe_tmp59 == 4u)) {    goto llvm_cbe_if_2e_then61;  } else {    goto llvm_cbe_if_2e_end62;  }


llvm_cbe_if_2e_then61:
////#line 207 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_this1));
////#line 207 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end62:
////#line 207 "small.cpp"
   // WARN;
////#line 209 "small.cpp"
  llvm_cbe_tmp63 = *(&llvm_cbe_ev_2e_addr);
////#line 209 "small.cpp"
  llvm_cbe_tmp65 = *((&llvm_cbe_tmp63->field1));
////#line 209 "small.cpp"
  if ((llvm_cbe_tmp65 == 5u)) {    goto llvm_cbe_if_2e_then67;  } else {    goto llvm_cbe_if_2e_end69;  }


llvm_cbe_if_2e_then67:
////#line 210 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)((&llvm_cbe_this1->field2))));
////#line 210 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end69:
////#line 210 "small.cpp"
   // WARN;
////#line 212 "small.cpp"
  *(&llvm_cbe_psx) = (((unsigned int *)llvm_cbe_this1));
////#line 212 "small.cpp"
   // ERROR;
////#line 214 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
////#line 214 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_return:
////#line 215 "small.cpp"
  llvm_cbe_tmp__3 = *(&llvm_cbe_retval);
////#line 215 "small.cpp"
  return llvm_cbe_tmp__3;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD0Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESD1Ev(llvm_cbe_this1);
////#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
////#line 139 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES13PrivateMemberEPii(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_i_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_y_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned int *llvm_cbe_tmp4;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp5;
  unsigned int llvm_cbe_tmp6;
  unsigned int llvm_cbe_call8;
  unsigned int llvm_cbe_call11;
  unsigned int llvm_cbe_tmp13;
  unsigned int llvm_cbe_call14;
  unsigned int llvm_cbe_tmp15;
  unsigned int *llvm_cbe_tmp18;
  unsigned char *llvm_cbe_tmp__4;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_y_2e_addr) = llvm_cbe_y;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 144 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
////#line 144 "small.cpp"
  *llvm_cbe_tmp = 0u;
////#line 144 "small.cpp"
   // WARN;
////#line 145 "small.cpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_i_2e_addr);
////#line 145 "small.cpp"
  llvm_cbe_tmp3 = *llvm_cbe_tmp2;
////#line 145 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_i_2e_addr);
////#line 145 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(llvm_cbe_tmp3, (((unsigned char *)llvm_cbe_tmp4)));
////#line 145 "small.cpp"
   // WARN;
////#line 146 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_i_2e_addr);
////#line 146 "small.cpp"
  llvm_cbe_tmp6 = *llvm_cbe_tmp5;
////#line 146 "small.cpp"
  llvm_cbe_call8 = _Z9API_CALL1iPv(llvm_cbe_tmp6, (((unsigned char *)(&llvm_cbe_y_2e_addr))));
////#line 146 "small.cpp"
   // WARN;
////#line 148 "small.cpp"
  llvm_cbe_call11 = _Z9API_CALL1iPv(0u, (((unsigned char *)((&llvm_cbe_this1->field2)))));
////#line 148 "small.cpp"
   // WARN;
////#line 149 "small.cpp"
  llvm_cbe_tmp13 = *((&llvm_cbe_this1->field2));
////#line 149 "small.cpp"
  llvm_cbe_call14 = _Z9API_CALL0i(llvm_cbe_tmp13);
////#line 149 "small.cpp"
   // WARN;
////#line 151 "small.cpp"
  llvm_cbe_tmp15 = *(&llvm_cbe_y_2e_addr);
////#line 151 "small.cpp"
  if ((llvm_cbe_tmp15 != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then:
////#line 153 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)((&llvm_cbe_this1->field2))));
////#line 153 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_if_2e_else:
////#line 156 "small.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_i_2e_addr);
////#line 156 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)llvm_cbe_tmp18));
////#line 156 "small.cpp"
  goto llvm_cbe_return;

llvm_cbe_return:
////#line 157 "small.cpp"
  llvm_cbe_tmp__4 = *(&llvm_cbe_retval);
////#line 157 "small.cpp"
  return llvm_cbe_tmp__4;
}


////#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES14PrivateMember2EPii(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_i_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_y_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp2;
  unsigned char *llvm_cbe_tmp__5;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_y_2e_addr) = llvm_cbe_y;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 161 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
////#line 161 "small.cpp"
  *llvm_cbe_tmp = 0u;
////#line 161 "small.cpp"
   // WARN;
////#line 163 "small.cpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_i_2e_addr);
////#line 163 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)llvm_cbe_tmp2));
////#line 164 "small.cpp"
  llvm_cbe_tmp__5 = *(&llvm_cbe_retval);
////#line 164 "small.cpp"
  return llvm_cbe_tmp__5;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3Log5doLogEPc(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned char *llvm_cbe_txt) {
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_txt_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_txt_2e_addr) = llvm_cbe_txt;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 89 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_txt_2e_addr);
////#line 89 "small.cpp"
  llvm_cbe_call = printf(llvm_cbe_tmp);
////#line 90 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace8SubUES_XE.array[((signed long long )2ull)]));
////#line 109 "small.cpp"
  *((&llvm_cbe_this1->field2)) = 0u;
////#line 109 "small.cpp"
   // WARN;
////#line 110 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_unnamed14 llvm_cbe_coerce;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;
  struct l_unnamed14 llvm_cbe_call;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_call6;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp7;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__6;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 114 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3Log13get_prifv_funEv((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])))));
////#line 114 "small.cpp"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_unnamed14 data; } *)(&llvm_cbe_coerce))->data = llvm_cbe_call;
////#line 114 "small.cpp"
   // WARN;
////#line 115 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field2);
////#line 115 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
////#line 115 "small.cpp"
  *llvm_cbe_tmp = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
////#line 115 "small.cpp"
   // WARN;
////#line 116 "small.cpp"
  llvm_cbe_tmp4 = *((&llvm_cbe_this1->field2));
////#line 116 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp4) % ((signed int )5u)))) != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
////#line 117 "small.cpp"
  llvm_cbe_call6 = _Z9API_CALL1iPv(0u, (((unsigned char *)((&llvm_cbe_this1->field2)))));
////#line 117 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
////#line 117 "small.cpp"
   // WARN;
////#line 120 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_ev_2e_addr);
////#line 120 "small.cpp"
  if ((llvm_cbe_tmp7 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
////#line 120 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp7)));
////#line 120 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
////#line 121 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
////#line 122 "small.cpp"
  llvm_cbe_tmp__6 = *(&llvm_cbe_retval);
////#line 122 "small.cpp"
  return llvm_cbe_tmp__6;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD0Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XD1Ev(llvm_cbe_this1);
////#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
////#line 110 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
struct l_unnamed14 _ZN12my_namespace3Log13get_prifv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) {
  struct l_unnamed14 llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  struct l_unnamed14 llvm_cbe_tmp__7;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 94 "small.cpp"
  *((&llvm_cbe_retval.field0)) = ((unsigned long long )(unsigned long)_ZN12my_namespace3Log11my_priv_funEv);
////#line 94 "small.cpp"
  *((&llvm_cbe_retval.field1)) = 0ull;
////#line 95 "small.cpp"
  llvm_cbe_tmp__7 = ((struct __attribute__ ((packed, aligned(1))) {struct l_unnamed14 data; } *)(&llvm_cbe_retval))->data;
////#line 95 "small.cpp"
  return llvm_cbe_tmp__7;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3Log11my_priv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 77 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESC2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_tmp__8;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
////#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__8 = ((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])));
////#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3UESE.array[((signed long long )2ull)]));
////#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = 0u;
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XC1Ev(((&llvm_cbe_this1->field4)));
////#line 134 "small.cpp"
  *((&llvm_cbe_this1->field5)) = ((unsigned int *)/*NULL*/0);
////#line 135 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3FSMC2Ev(struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this1;
  unsigned int llvm_cbe_call;
  unsigned int llvm_cbe_tmp;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8CBaseFSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))));
////#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3FSME.array[((signed long long )2ull)]));
////#line 65 "small.cpp"
  llvm_cbe_call = printf(((&_OC_str1.array[((signed int )0u)])));
////#line 66 "small.cpp"
  llvm_cbe_tmp = *(&_ZN12my_namespace8CBaseFSM5fubarE);
////#line 66 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = (((unsigned int )(((unsigned int )llvm_cbe_tmp) + ((unsigned int )1u))));
////#line 66 "small.cpp"
   // WARN;
////#line 67 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XC1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XC2Ev(llvm_cbe_this1);
////#line 105 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XC2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_tmp__9;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
////#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__9 = ((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])));
////#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace8SubUES_XE.array[((signed long long )2ull)]));
////#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = 0u;
////#line 105 "small.cpp"
  return;
}


////#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8CBaseFSMC2Ev(struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this1;

////#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
////#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
////#line 53 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = 0u;
////#line 53 "small.cpp"
   // WARN;
////#line 54 "small.cpp"
  return;
}

