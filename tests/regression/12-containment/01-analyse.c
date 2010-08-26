// PARAM: --analysis containment --class UES --allfuns CXX.json SAFE.json
/* I2 Goblint Version with debug info*/
#line 0 "LLVM INTERNAL"
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
struct l_class_OC_DummyStream;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM;
struct l_class_OC_my_namespace_KD__KD_Dummy;
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
typedef struct l_class_OC_DummyStream l_class_OC_DummyStream;
typedef struct l_class_OC_my_namespace_KD__KD_CBaseFSM l_class_OC_my_namespace_KD__KD_CBaseFSM;
typedef struct l_class_OC_my_namespace_KD__KD_Dummy l_class_OC_my_namespace_KD__KD_Dummy;
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
struct l_class_OC_DummyStream {  unsigned char field0;};

struct l_class_OC_my_namespace_KD__KD_CBaseFSM {  unsigned int field0;};

struct l_unnamed0 { unsigned char array[4]; };

struct l_class_OC_my_namespace_KD__KD_Dummy {  struct l_unnamed0 field0;};

struct l_unnamed15 { float array[4]; };

struct l_class_OC_my_namespace_KD__KD_FSM {  unsigned int  (**field0) ( int, ...);  struct l_unnamed0 field1;  struct l_unnamed15 field2;};

struct l_unnamed14 { unsigned int array[10]; };

struct l_class_OC_my_namespace_KD__KD_Log {  struct l_unnamed0 field0;  unsigned int field1;  struct l_unnamed14 field2;  struct l_unnamed14 field3;  unsigned int field4;};

struct l_unnamed2 { unsigned char array[28]; };

struct l_unnamed3 { unsigned char array[92]; };

struct l_class_OC_my_namespace_KD__KD_SubUES_X {  struct l_unnamed2 field0;  struct l_unnamed3 field1;  unsigned int field2;  struct l_unnamed0 field3;};

struct l_class_OC_my_namespace_KD__KD_UES {  struct l_unnamed2 field0;  struct l_unnamed3 field1;  unsigned int field2;  struct l_unnamed0 field3;  struct l_class_OC_my_namespace_KD__KD_SubUES_X field4;  unsigned int *field5;  unsigned int *field6;  unsigned int *field7;};

struct l_struct_OC_my_namespace_KD__KD_UEC_Event {  unsigned int field0;  unsigned int field1;};

struct l_unnamed1 { unsigned char array[5]; };

struct l_unnamed10 {  unsigned char *field0;  unsigned char *field1;};

struct l_unnamed11 { unsigned char array[26]; };

struct l_unnamed12 { unsigned char *array[5]; };

struct l_unnamed13 { unsigned char array[21]; };

struct l_unnamed4 { unsigned char array[15]; };

struct l_unnamed5 { unsigned char array[23]; };

struct l_unnamed6 { unsigned char *array[3]; };

struct l_unnamed7 { unsigned char array[10]; };

struct l_unnamed8 {  unsigned char *field0;  unsigned char *field1;  unsigned int field2;  unsigned int field3;  unsigned char *field4;  unsigned long long field5;  unsigned char *field6;  unsigned long long field7;};

struct l_unnamed9 {  unsigned char *field0;  unsigned char *field1;  unsigned int field2;  unsigned int field3;  unsigned char *field4;  unsigned long long field5;};


/* External Global Variable Declarations */
extern unsigned int API_0;
extern unsigned int API_1;
extern struct l_class_OC_DummyStream mout;
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
unsigned int *_Z19API_GET_GLOBAL_MEM2v(void);
void _ZN12my_namespace5dummyEv(void);
void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int __gxx_personality_v0(int vararg_dummy_arg,...);
void _ZSt9terminatev(void);
void _Unwind_Resume_or_Rethrow(unsigned char *);
void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest1EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest2EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest3EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest4EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest5EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned char *llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD0Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZdlPv(unsigned char *);
unsigned char *_ZN12my_namespace3UES13PrivateMemberEPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
unsigned char *_ZN12my_namespace3UES14PrivateMember2EPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_stack_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
unsigned int *_ZN12my_namespace3UES8RetTest6EPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pi) __ATTRIBUTE_WEAK__;
void _ZNK12my_namespace3Log5doLogEPc(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned char *llvm_cbe_txt) __ATTRIBUTE_WEAK__;
unsigned int *_Z11TS_API_SOMEPi(unsigned int *);
unsigned int _Z9API_CALL0i(unsigned int );
unsigned int printf(unsigned char *,...);
unsigned int _Z9API_CALL1iPv(unsigned int , unsigned char *);
unsigned int *_ZN12my_namespace3UES9undef_funEv(struct l_class_OC_my_namespace_KD__KD_UES *);
void _ZN12my_namespace8SubUES_XD2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD0Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
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
static struct l_unnamed14 _ZZ19API_GET_GLOBAL_MEM2vE2ma;
static struct l_unnamed7 _OC_str;
extern struct l_unnamed12 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed11 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed13 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed5 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed8 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__;
static struct l_unnamed4 _OC_str1;
static struct l_unnamed0 _OC_str2;
extern unsigned int _ZZNK12my_namespace3Log5doLogEPcE2kk __attribute__((common));
extern struct l_unnamed12 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed11 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed8 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed6 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__;
static struct l_unnamed1 _OC_str3;


/* Global Variable Definitions and Initialization */
unsigned int API_0;
unsigned int API_1;
static struct l_unnamed14 _ZZ19API_GET_GLOBAL_MEM2vE2ma;
static struct l_unnamed7 _OC_str = { "my_string" };
struct l_unnamed12 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3UESE)), ((unsigned char *)_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace3UESD1Ev), ((unsigned char *)_ZN12my_namespace3UESD0Ev) } };
struct l_unnamed13 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { "N12my_namespace3UESE" };
struct l_unnamed13 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { "N12my_namespace3FSME" };
struct l_unnamed11 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { "N12my_namespace8CBaseFSME" };
struct l_unnamed10 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8CBaseFSME.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3FSME.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace8CBaseFSME)), 2050ull };
struct l_unnamed13 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { "N12my_namespace3LogE" };
struct l_unnamed5 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { "N12my_namespace5Test2E" };
struct l_unnamed10 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace5Test2E.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3LogE.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace5Test2E)), 0ull };
struct l_unnamed8 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3UESE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 7170ull };
static struct l_unnamed4 _OC_str1 = { "received event" };
static struct l_unnamed0 _OC_str2 = { "%s\n" };
unsigned int _ZZNK12my_namespace3Log5doLogEPcE2kk __attribute__((common));
struct l_unnamed12 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace8SubUES_XE)), ((unsigned char *)_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace8SubUES_XD1Ev), ((unsigned char *)_ZN12my_namespace8SubUES_XD0Ev) } };
struct l_unnamed11 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { "N12my_namespace8SubUES_XE" };
struct l_unnamed8 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8SubUES_XE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 7170ull };
struct l_unnamed6 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3FSME)), ((unsigned char *)__cxa_pure_virtual) } };
static struct l_unnamed1 _OC_str3 = { "test" };


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

#line 0 "LLVM INTERNAL"
void _Z5__GWNv(void) {
#line 4 "regression.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _Z5__GERv(void) {
#line 5 "regression.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _Z5__GNWv(void) {
#line 6 "regression.h"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int *_Z19API_GET_GLOBAL_MEM2v(void) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp__1;

#line 74 "small.cpp"
  *(&llvm_cbe_retval) = ((&_ZZ19API_GET_GLOBAL_MEM2vE2ma.array[((signed int )0u)]));
#line 75 "small.cpp"
  llvm_cbe_tmp__1 = *(&llvm_cbe_retval);
#line 75 "small.cpp"
  return llvm_cbe_tmp__1;
}


void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this);

void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob);

#line 0 "LLVM INTERNAL"
void _ZN12my_namespace5dummyEv(void) {
  struct l_class_OC_my_namespace_KD__KD_UES llvm_cbe_ues;    /* Address-exposed local */
  unsigned int llvm_cbe_i;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp4;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call6;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp7;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call9;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp10;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call12;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp13;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_call15;

#line 321 "small.cpp"
  _ZN12my_namespace3UESC1Ev((&llvm_cbe_ues));
#line 323 "small.cpp"
  _ZN12my_namespace3UES16InsertGlobalDataEPi((&llvm_cbe_ues), (&llvm_cbe_i));
#line 327 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev);
#line 327 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3UES8RetTest1EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp);
#line 328 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_ev);
#line 328 "small.cpp"
  llvm_cbe_call6 = _ZN12my_namespace3UES8RetTest2EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp4);
#line 329 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_ev);
#line 329 "small.cpp"
  llvm_cbe_call9 = _ZN12my_namespace3UES8RetTest3EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp7);
#line 330 "small.cpp"
  llvm_cbe_tmp10 = *(&llvm_cbe_ev);
#line 330 "small.cpp"
  llvm_cbe_call12 = _ZN12my_namespace3UES8RetTest4EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp10);
#line 331 "small.cpp"
  llvm_cbe_tmp13 = *(&llvm_cbe_ev);
#line 331 "small.cpp"
  llvm_cbe_call15 = _ZN12my_namespace3UES8RetTest5EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp13);
#line 332 "small.cpp"
  _ZN12my_namespace3UESD1Ev((&llvm_cbe_ues));
#line 332 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESC1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESC2Ev(llvm_cbe_this1);
#line 209 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD1Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESD2Ev(llvm_cbe_this1);
#line 213 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UES16InsertGlobalDataEPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pglob) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pglob_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  struct l_class_OC_DummyStream *llvm_cbe_call;
  unsigned int *llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp4;
  struct l_class_OC_DummyStream *llvm_cbe_call5;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pglob_2e_addr) = llvm_cbe_pglob;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 277 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pglob_2e_addr);
#line 277 "small.cpp"
  *((&llvm_cbe_this1->field5)) = llvm_cbe_tmp;
#line 277 "small.cpp"
   // WARN;
#line 278 "small.cpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str.array[((signed int )0u)])));
#line 278 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_pglob_2e_addr);
#line 278 "small.cpp"
  llvm_cbe_tmp4 = *llvm_cbe_tmp3;
#line 278 "small.cpp"
  llvm_cbe_call5 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call, llvm_cbe_tmp4);
#line 278 "small.cpp"
   // NOWARN;
#line 279 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest1EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 257 "small.cpp"
   // WARN;
#line 257 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev_2e_addr);
#line 257 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_tmp));
#line 257 "small.cpp"
  llvm_cbe_tmp__2 = *(&llvm_cbe_retval);
#line 257 "small.cpp"
  return llvm_cbe_tmp__2;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest2EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp3;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__3;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 261 "small.cpp"
   // NOWARN;
#line 261 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev_2e_addr);
#line 261 "small.cpp"
  llvm_cbe_tmp3 = *((&llvm_cbe_tmp->field1));
#line 261 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((unsigned long long )(unsigned int )llvm_cbe_tmp3))));
#line 261 "small.cpp"
  llvm_cbe_tmp__3 = *(&llvm_cbe_retval);
#line 261 "small.cpp"
  return llvm_cbe_tmp__3;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest3EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__4;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 265 "small.cpp"
   // NOWARN;
#line 265 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field2));
#line 265 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((signed long long )(signed int )llvm_cbe_tmp2))));
#line 265 "small.cpp"
  llvm_cbe_tmp__4 = *(&llvm_cbe_retval);
#line 265 "small.cpp"
  return llvm_cbe_tmp__4;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest4EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__5;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 269 "small.cpp"
   // WARN;
#line 269 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_this1));
#line 269 "small.cpp"
  llvm_cbe_tmp__5 = *(&llvm_cbe_retval);
#line 269 "small.cpp"
  return llvm_cbe_tmp__5;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES8RetTest5EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__6;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 273 "small.cpp"
   // WARN;
#line 273 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)((&llvm_cbe_this1->field2))));
#line 273 "small.cpp"
  llvm_cbe_tmp__6 = *(&llvm_cbe_retval);
#line 273 "small.cpp"
  return llvm_cbe_tmp__6;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned char *llvm_cbe_arg) {
  struct l_class_OC_DummyStream *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this1;
  struct l_class_OC_DummyStream *llvm_cbe_tmp__7;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 54 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 55 "small.cpp"
  llvm_cbe_tmp__7 = *(&llvm_cbe_retval);
#line 55 "small.cpp"
  return llvm_cbe_tmp__7;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_DummyStream *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this1;
  struct l_class_OC_DummyStream *llvm_cbe_tmp__8;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 54 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 55 "small.cpp"
  llvm_cbe_tmp__8 = *(&llvm_cbe_retval);
#line 55 "small.cpp"
  return llvm_cbe_tmp__8;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3UESE.array[((signed long long )2ull)]));
#line 213 "small.cpp"
  _ZN12my_namespace8SubUES_XD1Ev(((&llvm_cbe_this1->field4)));
#line 213 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XD2Ev(llvm_cbe_this1);
#line 177 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_mi;    /* Address-exposed local */
  unsigned int *llvm_cbe_pno_ext;    /* Address-exposed local */
  unsigned int *llvm_cbe_psx;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Dummy llvm_cbe_d;    /* Address-exposed local */
  unsigned int *llvm_cbe_di;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp5;
  unsigned char *llvm_cbe_call;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp7;
  unsigned char *llvm_cbe_call9;
  unsigned int *llvm_cbe_call11;
  unsigned int *llvm_cbe_tmp13;
  unsigned int llvm_cbe_tmp14;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp15;
  unsigned int *llvm_cbe_tmp17;
  unsigned int llvm_cbe_tmp18;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp20;
  unsigned int llvm_cbe_tmp22;
  unsigned int *llvm_cbe_call26;
  unsigned int llvm_cbe_tmp28;
  unsigned int *llvm_cbe_tmp29;
  unsigned int llvm_cbe_tmp31;
  unsigned int llvm_cbe_call32;
  unsigned int llvm_cbe_tmp34;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp38;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__9;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 283 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field5));
#line 283 "small.cpp"
  llvm_cbe_tmp3 = *((&(*llvm_cbe_tmp2)));
#line 283 "small.cpp"
  *(&llvm_cbe_mi) = llvm_cbe_tmp3;
#line 283 "small.cpp"
   // WARN;
#line 285 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_ev_2e_addr);
#line 285 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3UES13PrivateMemberEPiiPj(llvm_cbe_this1, ((&llvm_cbe_this1->field2)), 0u, ((&llvm_cbe_tmp5->field1)));
#line 285 "small.cpp"
   // NOWARN;
#line 286 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_ev_2e_addr);
#line 286 "small.cpp"
  llvm_cbe_call9 = _ZN12my_namespace3UES14PrivateMember2EPiiPj(llvm_cbe_this1, (&llvm_cbe_mi), 0u, ((&llvm_cbe_tmp7->field1)));
#line 286 "small.cpp"
   // NOWARN;
#line 288 "small.cpp"
  llvm_cbe_call11 = _ZN12my_namespace3UES8RetTest6EPi(llvm_cbe_this1, ((&llvm_cbe_this1->field2)));
#line 288 "small.cpp"
   // NOWARN;
#line 290 "small.cpp"
  _ZNK12my_namespace3Log5doLogEPc((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])))), ((&_OC_str1.array[((signed int )0u)])));
#line 290 "small.cpp"
   // NOWARN;
#line 291 "small.cpp"
  llvm_cbe_tmp13 = (&(((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)]))))->field1);
#line 291 "small.cpp"
  llvm_cbe_tmp14 = *llvm_cbe_tmp13;
#line 291 "small.cpp"
  *llvm_cbe_tmp13 = (((unsigned int )(((unsigned int )llvm_cbe_tmp14) + ((unsigned int )1u))));
#line 291 "small.cpp"
   // NOWARN;
#line 293 "small.cpp"
  llvm_cbe_tmp15 = *(&llvm_cbe_ev_2e_addr);
#line 293 "small.cpp"
  *((&llvm_cbe_tmp15->field1)) = 0u;
#line 293 "small.cpp"
   // WARN;
#line 295 "small.cpp"
  llvm_cbe_tmp17 = (&llvm_cbe_this1->field2);
#line 295 "small.cpp"
  llvm_cbe_tmp18 = *llvm_cbe_tmp17;
#line 295 "small.cpp"
  *llvm_cbe_tmp17 = (((unsigned int )(((unsigned int )llvm_cbe_tmp18) + ((unsigned int )1u))));
#line 295 "small.cpp"
   // NOWARN;
#line 296 "small.cpp"
  llvm_cbe_tmp20 = *(&llvm_cbe_ev_2e_addr);
#line 296 "small.cpp"
  llvm_cbe_tmp22 = *((&llvm_cbe_tmp20->field1));
#line 296 "small.cpp"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp22;
#line 296 "small.cpp"
   // NOWARN;
#line 298 "small.cpp"
  llvm_cbe_call26 = _Z11TS_API_SOMEPi(((&llvm_cbe_this1->field2)));
#line 298 "small.cpp"
  *(&llvm_cbe_pno_ext) = llvm_cbe_call26;
#line 298 "small.cpp"
   // NOWARN;
#line 299 "small.cpp"
  llvm_cbe_tmp28 = *((&llvm_cbe_this1->field2));
#line 299 "small.cpp"
  llvm_cbe_tmp29 = *(&llvm_cbe_pno_ext);
#line 299 "small.cpp"
  *llvm_cbe_tmp29 = llvm_cbe_tmp28;
#line 299 "small.cpp"
   // NOWARN;
#line 301 "small.cpp"
  llvm_cbe_tmp31 = *((&llvm_cbe_this1->field2));
#line 301 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp31) % ((signed int )2u)))) == 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 302 "small.cpp"
  llvm_cbe_call32 = _Z9API_CALL0i(0u);
#line 302 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 302 "small.cpp"
   // NOWARN;
#line 303 "small.cpp"
  llvm_cbe_tmp34 = *((&llvm_cbe_this1->field2));
#line 303 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp34) % ((signed int )2u)))) != 0u)) {    goto llvm_cbe_if_2e_end39;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_else:
#line 309 "small.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_ev_2e_addr);
#line 309 "small.cpp"
  if ((llvm_cbe_tmp38 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end39;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 309 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp38)));
#line 309 "small.cpp"
  goto llvm_cbe_if_2e_end39;

llvm_cbe_if_2e_end39:
#line 309 "small.cpp"
   // NOWARN;
#line 311 "small.cpp"
  *(&llvm_cbe_psx) = (((unsigned int *)llvm_cbe_this1));
#line 311 "small.cpp"
   // ERROR;
#line 314 "small.cpp"
  *(&llvm_cbe_di) = (((unsigned int *)(&llvm_cbe_d)));
#line 314 "small.cpp"
   // NOWARN;
#line 316 "small.cpp"
   // NOWARN;
#line 316 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
#line 316 "small.cpp"
  llvm_cbe_tmp__9 = *(&llvm_cbe_retval);
#line 316 "small.cpp"
  return llvm_cbe_tmp__9;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESD0Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3UESD1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 213 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES13PrivateMemberEPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_i_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_y_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_glob_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_xx;    /* Address-exposed local */
  unsigned int *llvm_cbe_pxx;    /* Address-exposed local */
  unsigned int *llvm_cbe_pp;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp5;
  unsigned int *llvm_cbe_tmp6;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp7;
  unsigned int llvm_cbe_tmp8;
  unsigned int llvm_cbe_call10;
  unsigned int *llvm_cbe_tmp11;
  unsigned int llvm_cbe_call13;
  unsigned int llvm_cbe_call16;
  unsigned int *llvm_cbe_tmp17;
  unsigned int llvm_cbe_call19;
  unsigned int llvm_cbe_tmp21;
  unsigned int llvm_cbe_call22;
  unsigned int *llvm_cbe_call24;
  unsigned int *llvm_cbe_tmp25;
  unsigned int *llvm_cbe_tmp26;
  unsigned int *llvm_cbe_tmp27;
  unsigned int *llvm_cbe_tmp30;
  unsigned char *llvm_cbe_tmp__10;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_y_2e_addr) = llvm_cbe_y;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_glob_2e_addr) = llvm_cbe_glob;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 218 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
#line 218 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 218 "small.cpp"
   // NOWARN;
#line 221 "small.cpp"
  *(&llvm_cbe_pxx) = (&llvm_cbe_xx);
#line 223 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_i_2e_addr);
#line 223 "small.cpp"
  llvm_cbe_tmp5 = *llvm_cbe_tmp4;
#line 223 "small.cpp"
  llvm_cbe_tmp6 = *(&llvm_cbe_i_2e_addr);
#line 223 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(llvm_cbe_tmp5, (((unsigned char *)llvm_cbe_tmp6)));
#line 223 "small.cpp"
   // WARN;
#line 224 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_i_2e_addr);
#line 224 "small.cpp"
  llvm_cbe_tmp8 = *llvm_cbe_tmp7;
#line 224 "small.cpp"
  llvm_cbe_call10 = _Z9API_CALL1iPv(llvm_cbe_tmp8, (((unsigned char *)(&llvm_cbe_y_2e_addr))));
#line 224 "small.cpp"
   // NOWARN;
#line 226 "small.cpp"
  llvm_cbe_tmp11 = *(&llvm_cbe_pxx);
#line 226 "small.cpp"
  llvm_cbe_call13 = _Z9API_CALL1iPv(0u, (((unsigned char *)llvm_cbe_tmp11)));
#line 226 "small.cpp"
   // NOWARN;
#line 227 "small.cpp"
  llvm_cbe_call16 = _Z9API_CALL1iPv(0u, (((unsigned char *)((&llvm_cbe_this1->field2)))));
#line 227 "small.cpp"
   // WARN;
#line 228 "small.cpp"
  llvm_cbe_tmp17 = *(&llvm_cbe_glob_2e_addr);
#line 228 "small.cpp"
  llvm_cbe_call19 = _Z9API_CALL1iPv(0u, (((unsigned char *)llvm_cbe_tmp17)));
#line 228 "small.cpp"
   // WARN;
#line 229 "small.cpp"
  llvm_cbe_tmp21 = *((&llvm_cbe_this1->field2));
#line 229 "small.cpp"
  llvm_cbe_call22 = _Z9API_CALL0i(llvm_cbe_tmp21);
#line 229 "small.cpp"
   // NOWARN;
#line 231 "small.cpp"
  llvm_cbe_call24 = _ZN12my_namespace3UES9undef_funEv(llvm_cbe_this1);
#line 231 "small.cpp"
  *(&llvm_cbe_pp) = llvm_cbe_call24;
#line 231 "small.cpp"
   // WARN;
#line 232 "small.cpp"
  llvm_cbe_tmp25 = *(&llvm_cbe_pp);
#line 232 "small.cpp"
  *llvm_cbe_tmp25 = 0u;
#line 232 "small.cpp"
   // WARN;
#line 234 "small.cpp"
  llvm_cbe_tmp26 = *(&llvm_cbe_glob_2e_addr);
#line 234 "small.cpp"
  *llvm_cbe_tmp26 = 0u;
#line 234 "small.cpp"
   // WARN;
#line 237 "small.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_i_2e_addr);
#line 237 "small.cpp"
  *((&llvm_cbe_this1->field7)) = llvm_cbe_tmp27;
#line 237 "small.cpp"
   // NOWARN;
#line 238 "small.cpp"
  llvm_cbe_tmp30 = *((&llvm_cbe_this1->field7));
#line 238 "small.cpp"
  *llvm_cbe_tmp30 = 0u;
#line 238 "small.cpp"
   // NOWARN;
#line 240 "small.cpp"
   // NOWARN;
#line 240 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)((&llvm_cbe_this1->field2))));
#line 240 "small.cpp"
  llvm_cbe_tmp__10 = *(&llvm_cbe_retval);
#line 240 "small.cpp"
  return llvm_cbe_tmp__10;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES14PrivateMember2EPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_stack_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_stack_i_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_y_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_glob_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp3;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp4;
  unsigned char *llvm_cbe_tmp__11;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_stack_i_2e_addr) = llvm_cbe_stack_i;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_y_2e_addr) = llvm_cbe_y;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_glob_2e_addr) = llvm_cbe_glob;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 244 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_stack_i_2e_addr);
#line 244 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 244 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_stack_i_2e_addr);
#line 244 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(llvm_cbe_tmp2, (((unsigned char *)llvm_cbe_tmp3)));
#line 244 "small.cpp"
   // NOWARN;
#line 246 "small.cpp"
   // NOWARN;
#line 246 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_stack_i_2e_addr);
#line 246 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)llvm_cbe_tmp4));
#line 246 "small.cpp"
  llvm_cbe_tmp__11 = *(&llvm_cbe_retval);
#line 246 "small.cpp"
  return llvm_cbe_tmp__11;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZN12my_namespace3UES8RetTest6EPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pi) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pi_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp__12;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pi_2e_addr) = llvm_cbe_pi;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 250 "small.cpp"
   // NOWARN;
#line 250 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pi_2e_addr);
#line 250 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp;
#line 250 "small.cpp"
  llvm_cbe_tmp__12 = *(&llvm_cbe_retval);
#line 250 "small.cpp"
  return llvm_cbe_tmp__12;
}


#line 0 "LLVM INTERNAL"
void _ZNK12my_namespace3Log5doLogEPc(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned char *llvm_cbe_txt) {
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_txt_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  unsigned int llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_txt_2e_addr) = llvm_cbe_txt;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 147 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_txt_2e_addr);
#line 147 "small.cpp"
  llvm_cbe_call = printf(((&_OC_str2.array[((signed int )0u)])), llvm_cbe_tmp);
#line 147 "small.cpp"
   // NOWARN;
#line 149 "small.cpp"
  llvm_cbe_tmp2 = *(&_ZZNK12my_namespace3Log5doLogEPcE2kk);
#line 149 "small.cpp"
  *(&_ZZNK12my_namespace3Log5doLogEPcE2kk) = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 149 "small.cpp"
   // WARN;
#line 150 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace8SubUES_XE.array[((signed long long )2ull)]));
#line 176 "small.cpp"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 177 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_call;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp6;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__13;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 182 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field2);
#line 182 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 182 "small.cpp"
  *llvm_cbe_tmp = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 183 "small.cpp"
  llvm_cbe_tmp4 = *((&llvm_cbe_this1->field2));
#line 183 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp4) % ((signed int )5u)))) != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 184 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(0u, (((unsigned char *)((&llvm_cbe_this1->field2)))));
#line 184 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 186 "small.cpp"
  llvm_cbe_tmp6 = *(&llvm_cbe_ev_2e_addr);
#line 186 "small.cpp"
  if ((llvm_cbe_tmp6 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 186 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp6)));
#line 186 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
#line 186 "small.cpp"
   // NOWARN;
#line 187 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
#line 188 "small.cpp"
  llvm_cbe_tmp__13 = *(&llvm_cbe_retval);
#line 188 "small.cpp"
  return llvm_cbe_tmp__13;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XD0Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XD1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 177 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESC2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_tmp__14;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__14 = ((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3UESE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XC1Ev(((&llvm_cbe_this1->field4)));
#line 207 "small.cpp"
  *((&llvm_cbe_this1->field5)) = ((unsigned int *)/*NULL*/0);
#line 207 "small.cpp"
   // WARN;
#line 208 "small.cpp"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 208 "small.cpp"
   // NOWARN;
#line 209 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3FSMC2Ev(struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this1;
  unsigned int llvm_cbe_call;
  unsigned int llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8CBaseFSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3FSME.array[((signed long long )2ull)]));
#line 122 "small.cpp"
  llvm_cbe_call = printf(((&_OC_str3.array[((signed int )0u)])));
#line 122 "small.cpp"
   // NOWARN;
#line 123 "small.cpp"
  llvm_cbe_tmp = *(&_ZN12my_namespace8CBaseFSM5fubarE);
#line 123 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = (((unsigned int )(((unsigned int )llvm_cbe_tmp) + ((unsigned int )1u))));
#line 123 "small.cpp"
   // WARN;
#line 124 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XC1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XC2Ev(llvm_cbe_this1);
#line 172 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XC2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_tmp__15;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__15 = ((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )28ull)])));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace8SubUES_XE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 172 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8CBaseFSMC2Ev(struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 110 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = 0u;
#line 110 "small.cpp"
   // WARN;
#line 111 "small.cpp"
  return;
}

