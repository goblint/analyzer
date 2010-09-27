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
struct l_unnamed16;
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
typedef struct l_unnamed16 l_unnamed16;
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

struct l_unnamed15 { unsigned char array[4]; };

struct l_class_OC_my_namespace_KD__KD_Dummy {  struct l_unnamed15 field0;};

struct l_unnamed16 { float array[4]; };

struct l_class_OC_my_namespace_KD__KD_FSM {  unsigned int  (**field0) ( int, ...);  struct l_unnamed15 field1;  struct l_unnamed16 field2;};

struct l_unnamed14 {  unsigned long long field0;  unsigned long long field1;};

struct l_unnamed13 { unsigned int array[10]; };

struct l_class_OC_my_namespace_KD__KD_Log {  struct l_unnamed15 field0;  struct l_unnamed14 field1;  struct l_unnamed14 field2;  unsigned int field3;  struct l_unnamed13 field4;  unsigned int field5;  unsigned int *field6;  struct l_unnamed13 field7;  unsigned int field8;};

struct l_unnamed1 { unsigned char array[28]; };

struct l_unnamed0 { unsigned char array[140]; };

struct l_class_OC_my_namespace_KD__KD_SubUES_X {  struct l_unnamed1 field0;  struct l_unnamed15 field1;  struct l_unnamed0 field2;  unsigned int field3;};

struct l_class_OC_my_namespace_KD__KD_UES {  struct l_unnamed1 field0;  struct l_unnamed15 field1;  struct l_unnamed0 field2;  unsigned int field3;  unsigned int field4;  struct l_unnamed15 field5;  struct l_class_OC_my_namespace_KD__KD_SubUES_X field6;  unsigned int *field7;  unsigned int *field8;  unsigned int *field9;  unsigned int *field10;  unsigned int *field11;  unsigned int *field12;  unsigned int **field13;  unsigned int *field14;};

struct l_struct_OC_my_namespace_KD__KD_UEC_Event {  unsigned int field0;  unsigned int field1;};

struct l_unnamed10 { unsigned char array[26]; };

struct l_unnamed11 { unsigned char *array[5]; };

struct l_unnamed12 { unsigned char array[21]; };

struct l_unnamed2 { unsigned char *array[3]; };

struct l_unnamed3 { unsigned char array[15]; };

struct l_unnamed4 { unsigned char array[23]; };

struct l_unnamed5 { unsigned char array[10]; };

struct l_unnamed6 {  unsigned char *field0;  unsigned char *field1;};

struct l_unnamed7 { unsigned char array[5]; };

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
unsigned int *_ZN12my_namespace3UES8RetTest7EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UES5EntryEv(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3Log7call_fpEPi(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UES14PrivateMember5Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int *_Z18API_GET_GLOBAL_MEMv(void);
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned char *llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESD0Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZdlPv(unsigned char *);
unsigned int *_ZN12my_namespace3UES7GetGlobEv(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _Z8COPY_BADPPiS_(unsigned int **, unsigned int *);
unsigned char *_ZN12my_namespace3UES13PrivateMemberEPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
unsigned char *_ZN12my_namespace3UES14PrivateMember2EPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_stack_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
unsigned char *_ZN12my_namespace3UES14PrivateMember3EPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_glob_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) __ATTRIBUTE_WEAK__;
unsigned char *_ZN12my_namespace3UES14PrivateMember4EPPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int **llvm_cbe_ipppppq) __ATTRIBUTE_WEAK__;
unsigned int *_ZN12my_namespace3UES8RetTest6EPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pi) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3Log5doLogEPc(unsigned char *llvm_cbe_txt) __ATTRIBUTE_WEAK__;
unsigned int *_Z11TS_API_SOMEPi(unsigned int *);
unsigned int _Z9API_CALL0i(unsigned int );
void _ZN12my_namespace8SubUES_X3ADDEv(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_unnamed14 _ZN12my_namespace3Log12get_priv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int *_ZN12my_namespace3Log11my_priv_funEPi(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned int *llvm_cbe_pi) __ATTRIBUTE_WEAK__;
unsigned int printf(unsigned char *,...);
unsigned int _Z10XXXXXXXXXXiPv(unsigned int , unsigned char *);
unsigned int *_ZN12my_namespace6memcpyEPiS0_i(unsigned int *, unsigned int *, unsigned int );
unsigned int _Z9API_CALL1iPv(unsigned int , unsigned char *);
unsigned int *_ZN12my_namespace3UES9undef_funEv(struct l_class_OC_my_namespace_KD__KD_UES *);
void _ZN12my_namespace8SubUES_XD2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XD0Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3UESC2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3FSMC2Ev(struct l_class_OC_my_namespace_KD__KD_FSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace3LogC2Ev(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XC1Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12my_namespace8SubUES_XC2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int *_ZN12my_namespace3Log15my_undef_fp_funEPi(struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
void _ZN12my_namespace8CBaseFSMC2Ev(struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void __cxa_pure_virtual(void);
void abort(void);
unsigned char *memcpy(unsigned char *, unsigned char *, unsigned long long );


/* Global Variable Declarations */
extern unsigned int API_0;
extern unsigned int API_1;
static struct l_unnamed13 _ZZ19API_GET_GLOBAL_MEM2vE2ma;
static struct l_unnamed5 _OC_str;
extern struct l_unnamed11 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed12 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__;
extern struct l_unnamed12 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed6 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__;
extern struct l_unnamed12 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed4 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed6 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__;
extern struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__;
extern struct l_unnamed8 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__;
static struct l_unnamed3 _OC_str1;
extern unsigned int _ZZN12my_namespace3Log11my_priv_funEPiE2xx __attribute__((common));
static struct l_unnamed15 _OC_str2;
extern unsigned int _ZZN12my_namespace3Log5doLogEPcE2kk __attribute__((common));
extern struct l_unnamed11 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed10 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed8 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__;
extern struct l_unnamed2 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__;
static struct l_unnamed7 _OC_str3;


/* Global Variable Definitions and Initialization */
unsigned int API_0;
unsigned int API_1;
static struct l_unnamed13 _ZZ19API_GET_GLOBAL_MEM2vE2ma;
static struct l_unnamed5 _OC_str = { "my_string" };
struct l_unnamed11 _ZTVN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3UESE)), ((unsigned char *)_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace3UESD1Ev), ((unsigned char *)_ZN12my_namespace3UESD0Ev) } };
struct l_unnamed12 _ZTSN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { "N12my_namespace3UESE" };
struct l_unnamed12 _ZTSN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { "N12my_namespace3FSME" };
struct l_unnamed10 _ZTSN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { "N12my_namespace8CBaseFSME" };
struct l_unnamed6 _ZTIN12my_namespace8CBaseFSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8CBaseFSME.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3FSME.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace8CBaseFSME)), 2050ull };
struct l_unnamed12 _ZTSN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { "N12my_namespace3LogE" };
struct l_unnamed4 _ZTSN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { "N12my_namespace5Test2E" };
struct l_unnamed6 _ZTIN12my_namespace5Test2E __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace5Test2E.array[((signed int )0u)])) };
struct l_unnamed9 _ZTIN12my_namespace3LogE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3LogE.array[((signed int )0u)])), 0u, 1u, ((unsigned char *)(&_ZTIN12my_namespace5Test2E)), 0ull };
struct l_unnamed8 _ZTIN12my_namespace3UESE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace3UESE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 8194ull };
static struct l_unnamed3 _OC_str1 = { "received event" };
unsigned int _ZZN12my_namespace3Log11my_priv_funEPiE2xx __attribute__((common));
static struct l_unnamed15 _OC_str2 = { "%s\n" };
unsigned int _ZZN12my_namespace3Log5doLogEPcE2kk __attribute__((common));
struct l_unnamed11 _ZTVN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace8SubUES_XE)), ((unsigned char *)_ZN12my_namespace8SubUES_X12ReceiveEventEPNS_9UEC_EventE), ((unsigned char *)_ZN12my_namespace8SubUES_XD1Ev), ((unsigned char *)_ZN12my_namespace8SubUES_XD0Ev) } };
struct l_unnamed10 _ZTSN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { "N12my_namespace8SubUES_XE" };
struct l_unnamed8 _ZTIN12my_namespace8SubUES_XE __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv121__vmi_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTSN12my_namespace8SubUES_XE.array[((signed int )0u)])), 0u, 2u, ((unsigned char *)(&_ZTIN12my_namespace3FSME)), 2ull, ((unsigned char *)(&_ZTIN12my_namespace3LogE)), 8194ull };
struct l_unnamed2 _ZTVN12my_namespace3FSME __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTIN12my_namespace3FSME)), ((unsigned char *)__cxa_pure_virtual) } };
static struct l_unnamed7 _OC_str3 = { "test" };


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

#line 75 "small.cpp"
  *(&llvm_cbe_retval) = ((&_ZZ19API_GET_GLOBAL_MEM2vE2ma.array[((signed int )0u)]));
#line 76 "small.cpp"
  llvm_cbe_tmp__1 = *(&llvm_cbe_retval);
#line 76 "small.cpp"
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
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp16;
  unsigned int *llvm_cbe_call18;

#line 482 "small.cpp"
  _ZN12my_namespace3UESC1Ev((&llvm_cbe_ues));
#line 484 "small.cpp"
  _ZN12my_namespace3UES16InsertGlobalDataEPi((&llvm_cbe_ues), (&llvm_cbe_i));
#line 488 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev);
#line 488 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3UES8RetTest1EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp);
#line 489 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_ev);
#line 489 "small.cpp"
  llvm_cbe_call6 = _ZN12my_namespace3UES8RetTest2EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp4);
#line 490 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_ev);
#line 490 "small.cpp"
  llvm_cbe_call9 = _ZN12my_namespace3UES8RetTest3EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp7);
#line 491 "small.cpp"
  llvm_cbe_tmp10 = *(&llvm_cbe_ev);
#line 491 "small.cpp"
  llvm_cbe_call12 = _ZN12my_namespace3UES8RetTest4EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp10);
#line 492 "small.cpp"
  llvm_cbe_tmp13 = *(&llvm_cbe_ev);
#line 492 "small.cpp"
  llvm_cbe_call15 = _ZN12my_namespace3UES8RetTest5EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp13);
#line 494 "small.cpp"
  llvm_cbe_tmp16 = *(&llvm_cbe_ev);
#line 494 "small.cpp"
  llvm_cbe_call18 = _ZN12my_namespace3UES8RetTest7EPNS_9UEC_EventE((&llvm_cbe_ues), llvm_cbe_tmp16);
#line 495 "small.cpp"
  _ZN12my_namespace3UES5EntryEv((&llvm_cbe_ues));
#line 497 "small.cpp"
  _ZN12my_namespace3Log7call_fpEPi((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)(&llvm_cbe_ues)))[((signed long long )32ull)])))), ((unsigned int *)/*NULL*/0));
#line 498 "small.cpp"
  _ZN12my_namespace3UESD1Ev((&llvm_cbe_ues));
#line 498 "small.cpp"
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
#line 266 "small.cpp"
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
#line 270 "small.cpp"
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
#line 412 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pglob_2e_addr);
#line 412 "small.cpp"
  *((&llvm_cbe_this1->field7)) = llvm_cbe_tmp;
#line 412 "small.cpp"
   // WARN;
#line 413 "small.cpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str.array[((signed int )0u)])));
#line 413 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_pglob_2e_addr);
#line 413 "small.cpp"
  llvm_cbe_tmp4 = *llvm_cbe_tmp3;
#line 413 "small.cpp"
  llvm_cbe_call5 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call, llvm_cbe_tmp4);
#line 413 "small.cpp"
   // NOWARN;
#line 414 "small.cpp"
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
#line 391 "small.cpp"
   // WARN;
#line 391 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev_2e_addr);
#line 391 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_tmp));
#line 391 "small.cpp"
  llvm_cbe_tmp__2 = *(&llvm_cbe_retval);
#line 391 "small.cpp"
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
#line 395 "small.cpp"
   // NOWARN;
#line 395 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ev_2e_addr);
#line 395 "small.cpp"
  llvm_cbe_tmp3 = *((&llvm_cbe_tmp->field1));
#line 395 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((unsigned long long )(unsigned int )llvm_cbe_tmp3))));
#line 395 "small.cpp"
  llvm_cbe_tmp__3 = *(&llvm_cbe_retval);
#line 395 "small.cpp"
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
#line 399 "small.cpp"
   // NOWARN;
#line 399 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field3));
#line 399 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)(unsigned long)(((signed long long )(signed int )llvm_cbe_tmp2))));
#line 399 "small.cpp"
  llvm_cbe_tmp__4 = *(&llvm_cbe_retval);
#line 399 "small.cpp"
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
#line 403 "small.cpp"
   // WARN;
#line 403 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_this1));
#line 403 "small.cpp"
  llvm_cbe_tmp__5 = *(&llvm_cbe_retval);
#line 403 "small.cpp"
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
#line 407 "small.cpp"
   // WARN;
#line 407 "small.cpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)((&llvm_cbe_this1->field3))));
#line 407 "small.cpp"
  llvm_cbe_tmp__6 = *(&llvm_cbe_retval);
#line 407 "small.cpp"
  return llvm_cbe_tmp__6;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZN12my_namespace3UES8RetTest7EPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pg;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_call;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp__7;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 385 "small.cpp"
  llvm_cbe_call = _Z18API_GET_GLOBAL_MEMv();
#line 385 "small.cpp"
  *(&llvm_cbe_pg) = llvm_cbe_call;
#line 386 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pg);
#line 386 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 386 "small.cpp"
   // WARN;
#line 387 "small.cpp"
   // WARN;
#line 387 "small.cpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_pg);
#line 387 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 387 "small.cpp"
  llvm_cbe_tmp__7 = *(&llvm_cbe_retval);
#line 387 "small.cpp"
  return llvm_cbe_tmp__7;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UES5EntryEv(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 380 "small.cpp"
  _ZN12my_namespace3UES14PrivateMember5Ev(llvm_cbe_this1);
#line 381 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3Log7call_fpEPi(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned int *llvm_cbe_glob) {
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_glob_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pg;    /* Address-exposed local */
  struct l_unnamed14 llvm_cbe_mem_2e_fn;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__8;
  unsigned long long llvm_cbe_mem_2e_fn_2e_adj;
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this4;
  unsigned long long *llvm_cbe_mem_2e_fn_2e_ptr;
  unsigned long long llvm_cbe_fn;
  unsigned int * (**llvm_cbe_tmp__9) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
  unsigned int * (*llvm_cbe_virtualfn) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
  unsigned long long llvm_cbe_fn6;
  unsigned int * (*llvm_cbe_tmp__10) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
  unsigned int * (*llvm_cbe_tmp__11) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
  unsigned int * (*llvm_cbe_tmp__11__PHI_TEMPORARY) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *);
  unsigned int *llvm_cbe_tmp7;
  unsigned int *llvm_cbe_call;
  unsigned int *llvm_cbe_tmp8;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_glob_2e_addr) = llvm_cbe_glob;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__8 = memcpy((((unsigned char *)(&llvm_cbe_mem_2e_fn))), (((unsigned char *)((&llvm_cbe_this1->field1)))), 16ull);
#line 189 "small.cpp"
  llvm_cbe_mem_2e_fn_2e_adj = *((&llvm_cbe_mem_2e_fn.field1));
#line 189 "small.cpp"
  llvm_cbe_this4 = ((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )llvm_cbe_mem_2e_fn_2e_adj)])));
#line 189 "small.cpp"
  llvm_cbe_mem_2e_fn_2e_ptr = (&llvm_cbe_mem_2e_fn.field0);
#line 189 "small.cpp"
  llvm_cbe_fn = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 189 "small.cpp"
  if ((((((bool )(llvm_cbe_fn & 1ull)&1u))&1))) {    goto llvm_cbe_fn_2e_virtual;  } else {    goto llvm_cbe_fn_2e_nonvirtual;  }


llvm_cbe_fn_2e_virtual:
#line 189 "small.cpp"
  llvm_cbe_tmp__9 = *(((unsigned int * (***) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *))llvm_cbe_this4));
#line 189 "small.cpp"
  llvm_cbe_virtualfn = *(((unsigned int * (**) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *))((&(((unsigned char *)llvm_cbe_tmp__9))[((signed long long )(((unsigned long long )(((unsigned long long )llvm_cbe_fn) - ((unsigned long long )1ull)))))]))));
#line 189 "small.cpp"
  llvm_cbe_tmp__11__PHI_TEMPORARY = llvm_cbe_virtualfn;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_nonvirtual:
#line 189 "small.cpp"
  llvm_cbe_fn6 = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 189 "small.cpp"
  llvm_cbe_tmp__10 = ((unsigned int * (*) (struct l_class_OC_my_namespace_KD__KD_Log *, unsigned int *))(unsigned long)llvm_cbe_fn6);
#line 189 "small.cpp"
  llvm_cbe_tmp__11__PHI_TEMPORARY = llvm_cbe_tmp__10;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_end:
#line 189 "small.cpp"
  llvm_cbe_tmp__11 = llvm_cbe_tmp__11__PHI_TEMPORARY;
#line 189 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_glob_2e_addr);
#line 189 "small.cpp"
  llvm_cbe_call = llvm_cbe_tmp__11(llvm_cbe_this4, llvm_cbe_tmp7);
#line 189 "small.cpp"
  *(&llvm_cbe_pg) = llvm_cbe_call;
#line 189 "small.cpp"
   // WARN;
#line 190 "small.cpp"
  llvm_cbe_tmp8 = *(&llvm_cbe_pg);
#line 190 "small.cpp"
  *llvm_cbe_tmp8 = 0u;
#line 190 "small.cpp"
   // WARN;
#line 192 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UES14PrivateMember5Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int **llvm_cbe_pl;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_call;
  unsigned int **llvm_cbe_tmp3;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 369 "small.cpp"
  *(&llvm_cbe_pl) = ((&llvm_cbe_this1->field14));
#line 369 "small.cpp"
   // WARN;
#line 370 "small.cpp"
  *((&llvm_cbe_this1->field14)) = ((unsigned int *)/*NULL*/0);
#line 370 "small.cpp"
   // WARN;
#line 371 "small.cpp"
  llvm_cbe_call = _Z18API_GET_GLOBAL_MEMv();
#line 371 "small.cpp"
  *(&llvm_cbe_pl) = (((unsigned int **)llvm_cbe_call));
#line 371 "small.cpp"
   // WARN;
#line 372 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_pl);
#line 372 "small.cpp"
  *llvm_cbe_tmp3 = ((unsigned int *)/*NULL*/0);
#line 372 "small.cpp"
   // WARN;
#line 373 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned char *llvm_cbe_arg) {
  struct l_class_OC_DummyStream *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this1;
  struct l_class_OC_DummyStream *llvm_cbe_tmp__12;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 53 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 54 "small.cpp"
  llvm_cbe_tmp__12 = *(&llvm_cbe_retval);
#line 54 "small.cpp"
  return llvm_cbe_tmp__12;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_DummyStream *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_DummyStream *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_DummyStream *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_DummyStream *llvm_cbe_this1;
  struct l_class_OC_DummyStream *llvm_cbe_tmp__13;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 53 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 54 "small.cpp"
  llvm_cbe_tmp__13 = *(&llvm_cbe_retval);
#line 54 "small.cpp"
  return llvm_cbe_tmp__13;
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
#line 270 "small.cpp"
  _ZN12my_namespace8SubUES_XD1Ev(((&llvm_cbe_this1->field6)));
#line 270 "small.cpp"
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
#line 225 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_my_namespace_KD__KD_CBaseFSM *_ZN12my_namespace3UES12ReceiveEventEPNS_9UEC_EventE(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev) {
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_mi;    /* Address-exposed local */
  unsigned int *llvm_cbe_will_be_glob;    /* Address-exposed local */
  unsigned int *llvm_cbe_might_be_glob;    /* Address-exposed local */
  unsigned int *llvm_cbe_p1;    /* Address-exposed local */
  unsigned int *llvm_cbe_p2;    /* Address-exposed local */
  unsigned int *llvm_cbe_pno_ext;    /* Address-exposed local */
  unsigned int *llvm_cbe_psx;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Dummy llvm_cbe_d;    /* Address-exposed local */
  unsigned int *llvm_cbe_di;    /* Address-exposed local */
  struct l_unnamed14 llvm_cbe_coerce;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_call;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  unsigned int *llvm_cbe_tmp7;
  unsigned int *llvm_cbe_tmp8;
  unsigned int *llvm_cbe_tmp11;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp15;
  unsigned char *llvm_cbe_call17;
  unsigned int *llvm_cbe_tmp18;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp20;
  unsigned char *llvm_cbe_call22;
  unsigned int *llvm_cbe_tmp23;
  unsigned int *llvm_cbe_tmp24;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp25;
  unsigned char *llvm_cbe_call27;
  unsigned char *llvm_cbe_call29;
  unsigned int *llvm_cbe_call31;
  unsigned int *llvm_cbe_tmp32;
  unsigned int llvm_cbe_tmp33;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp34;
  unsigned int *llvm_cbe_tmp36;
  unsigned int llvm_cbe_tmp37;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp39;
  unsigned int llvm_cbe_tmp41;
  unsigned int *llvm_cbe_call45;
  unsigned int llvm_cbe_tmp47;
  unsigned int *llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp50;
  unsigned int llvm_cbe_call51;
  unsigned int llvm_cbe_tmp53;
  struct l_struct_OC_my_namespace_KD__KD_UEC_Event *llvm_cbe_tmp58;
  struct l_unnamed14 llvm_cbe_call64;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__14;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 418 "small.cpp"
  llvm_cbe_call = _ZN12my_namespace3UES7GetGlobEv(llvm_cbe_this1);
#line 418 "small.cpp"
  *((&(*llvm_cbe_call))) = 0u;
#line 418 "small.cpp"
   // WARN;
#line 420 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field7));
#line 420 "small.cpp"
  llvm_cbe_tmp4 = *((&(*llvm_cbe_tmp2)));
#line 420 "small.cpp"
  *(&llvm_cbe_mi) = llvm_cbe_tmp4;
#line 420 "small.cpp"
   // WARN;
#line 423 "small.cpp"
  llvm_cbe_tmp7 = *((&llvm_cbe_this1->field7));
#line 423 "small.cpp"
  _Z8COPY_BADPPiS_((&llvm_cbe_will_be_glob), llvm_cbe_tmp7);
#line 423 "small.cpp"
   // WARN;
#line 425 "small.cpp"
  llvm_cbe_tmp8 = *(&llvm_cbe_will_be_glob);
#line 425 "small.cpp"
  *((&(*llvm_cbe_tmp8))) = 0u;
#line 425 "small.cpp"
   // WARN;
#line 428 "small.cpp"
  _Z8COPY_BADPPiS_((&llvm_cbe_might_be_glob), ((unsigned int *)/*NULL*/0));
#line 429 "small.cpp"
  llvm_cbe_tmp11 = *(&llvm_cbe_might_be_glob);
#line 429 "small.cpp"
  *((&(*llvm_cbe_tmp11))) = 0u;
#line 429 "small.cpp"
   // WARN;
#line 431 "small.cpp"
  llvm_cbe_tmp15 = *(&llvm_cbe_ev_2e_addr);
#line 431 "small.cpp"
  llvm_cbe_call17 = _ZN12my_namespace3UES13PrivateMemberEPiiPj(llvm_cbe_this1, ((&llvm_cbe_this1->field3)), 0u, ((&llvm_cbe_tmp15->field1)));
#line 431 "small.cpp"
  *(&llvm_cbe_p1) = (((unsigned int *)llvm_cbe_call17));
#line 431 "small.cpp"
   // NOWARN;
#line 434 "small.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_p1);
#line 434 "small.cpp"
  *llvm_cbe_tmp18 = 0u;
#line 434 "small.cpp"
   // WARN;
#line 436 "small.cpp"
  llvm_cbe_tmp20 = *(&llvm_cbe_ev_2e_addr);
#line 436 "small.cpp"
  llvm_cbe_call22 = _ZN12my_namespace3UES14PrivateMember2EPiiPj(llvm_cbe_this1, (&llvm_cbe_mi), 0u, ((&llvm_cbe_tmp20->field1)));
#line 436 "small.cpp"
  *(&llvm_cbe_p2) = (((unsigned int *)llvm_cbe_call22));
#line 436 "small.cpp"
   // NOWARN;
#line 438 "small.cpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_p2);
#line 438 "small.cpp"
  *llvm_cbe_tmp23 = 0u;
#line 438 "small.cpp"
   // WARN;
#line 440 "small.cpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_will_be_glob);
#line 440 "small.cpp"
  llvm_cbe_tmp25 = *(&llvm_cbe_ev_2e_addr);
#line 440 "small.cpp"
  llvm_cbe_call27 = _ZN12my_namespace3UES14PrivateMember3EPiiPj(llvm_cbe_this1, llvm_cbe_tmp24, 0u, ((&llvm_cbe_tmp25->field1)));
#line 440 "small.cpp"
   // NOWARN;
#line 442 "small.cpp"
  llvm_cbe_call29 = _ZN12my_namespace3UES14PrivateMember4EPPi(llvm_cbe_this1, ((&llvm_cbe_this1->field10)));
#line 442 "small.cpp"
   // WARN;
#line 445 "small.cpp"
  llvm_cbe_call31 = _ZN12my_namespace3UES8RetTest6EPi(llvm_cbe_this1, ((&llvm_cbe_this1->field3)));
#line 445 "small.cpp"
   // NOWARN;
#line 447 "small.cpp"
  _ZN12my_namespace3Log5doLogEPc(((&_OC_str1.array[((signed int )0u)])));
#line 447 "small.cpp"
   // NOWARN;
#line 448 "small.cpp"
  llvm_cbe_tmp32 = (&(((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )32ull)]))))->field3);
#line 448 "small.cpp"
  llvm_cbe_tmp33 = *llvm_cbe_tmp32;
#line 448 "small.cpp"
  *llvm_cbe_tmp32 = (((unsigned int )(((unsigned int )llvm_cbe_tmp33) + ((unsigned int )1u))));
#line 448 "small.cpp"
   // NOWARN;
#line 450 "small.cpp"
  llvm_cbe_tmp34 = *(&llvm_cbe_ev_2e_addr);
#line 450 "small.cpp"
  *((&llvm_cbe_tmp34->field1)) = 0u;
#line 450 "small.cpp"
   // WARN;
#line 452 "small.cpp"
  llvm_cbe_tmp36 = (&llvm_cbe_this1->field3);
#line 452 "small.cpp"
  llvm_cbe_tmp37 = *llvm_cbe_tmp36;
#line 452 "small.cpp"
  *llvm_cbe_tmp36 = (((unsigned int )(((unsigned int )llvm_cbe_tmp37) + ((unsigned int )1u))));
#line 452 "small.cpp"
   // NOWARN;
#line 453 "small.cpp"
  llvm_cbe_tmp39 = *(&llvm_cbe_ev_2e_addr);
#line 453 "small.cpp"
  llvm_cbe_tmp41 = *((&llvm_cbe_tmp39->field1));
#line 453 "small.cpp"
  *((&llvm_cbe_this1->field3)) = llvm_cbe_tmp41;
#line 453 "small.cpp"
   // NOWARN;
#line 455 "small.cpp"
  llvm_cbe_call45 = _Z11TS_API_SOMEPi(((&llvm_cbe_this1->field3)));
#line 455 "small.cpp"
  *(&llvm_cbe_pno_ext) = llvm_cbe_call45;
#line 455 "small.cpp"
   // NOWARN;
#line 456 "small.cpp"
  llvm_cbe_tmp47 = *((&llvm_cbe_this1->field3));
#line 456 "small.cpp"
  llvm_cbe_tmp48 = *(&llvm_cbe_pno_ext);
#line 456 "small.cpp"
  *llvm_cbe_tmp48 = llvm_cbe_tmp47;
#line 456 "small.cpp"
   // NOWARN;
#line 458 "small.cpp"
  llvm_cbe_tmp50 = *((&llvm_cbe_this1->field3));
#line 458 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp50) % ((signed int )2u)))) == 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 459 "small.cpp"
  llvm_cbe_call51 = _Z9API_CALL0i(0u);
#line 459 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 459 "small.cpp"
   // NOWARN;
#line 462 "small.cpp"
  llvm_cbe_tmp53 = *((&llvm_cbe_this1->field3));
#line 462 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp53) % ((signed int )2u)))) != 0u)) {    goto llvm_cbe_if_2e_then56;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then56:
#line 464 "small.cpp"
  _ZN12my_namespace8SubUES_X3ADDEv(((&llvm_cbe_this1->field6)));
#line 466 "small.cpp"
  goto llvm_cbe_if_2e_end59;

llvm_cbe_if_2e_else:
#line 468 "small.cpp"
  llvm_cbe_tmp58 = *(&llvm_cbe_ev_2e_addr);
#line 468 "small.cpp"
  if ((llvm_cbe_tmp58 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end59;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 468 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp58)));
#line 468 "small.cpp"
  goto llvm_cbe_if_2e_end59;

llvm_cbe_if_2e_end59:
#line 468 "small.cpp"
   // NOWARN;
#line 470 "small.cpp"
  *(&llvm_cbe_psx) = (((unsigned int *)llvm_cbe_this1));
#line 470 "small.cpp"
   // ERROR;
#line 473 "small.cpp"
  *(&llvm_cbe_di) = (((unsigned int *)(&llvm_cbe_d)));
#line 473 "small.cpp"
   // NOWARN;
#line 475 "small.cpp"
  llvm_cbe_call64 = _ZN12my_namespace3Log12get_priv_funEv((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )32ull)])))));
#line 475 "small.cpp"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_unnamed14 data; } *)(&llvm_cbe_coerce))->data = llvm_cbe_call64;
#line 475 "small.cpp"
   // NOWARN;
#line 477 "small.cpp"
   // NOWARN;
#line 477 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
#line 477 "small.cpp"
  llvm_cbe_tmp__14 = *(&llvm_cbe_retval);
#line 477 "small.cpp"
  return llvm_cbe_tmp__14;
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
#line 270 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZN12my_namespace3UES7GetGlobEv(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp__15;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 365 "small.cpp"
   // WARN;
#line 365 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field7));
#line 365 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 365 "small.cpp"
  llvm_cbe_tmp__15 = *(&llvm_cbe_retval);
#line 365 "small.cpp"
  return llvm_cbe_tmp__15;
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
  unsigned int *llvm_cbe_mlpi;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp5;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp6;
  unsigned int llvm_cbe_call8;
  unsigned int *llvm_cbe_tmp9;
  unsigned int llvm_cbe_call11;
  unsigned int llvm_cbe_tmp13;
  unsigned int llvm_cbe_call14;
  unsigned int *llvm_cbe_call16;
  unsigned int *llvm_cbe_tmp17;
  unsigned int *llvm_cbe_tmp18;
  unsigned int llvm_cbe_call21;
  unsigned int *llvm_cbe_tmp22;
  unsigned char *llvm_cbe_tmp__16;

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
#line 314 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
#line 314 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 314 "small.cpp"
   // NOWARN;
#line 317 "small.cpp"
  *(&llvm_cbe_pxx) = (&llvm_cbe_xx);
#line 319 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_i_2e_addr);
#line 319 "small.cpp"
  llvm_cbe_tmp5 = *llvm_cbe_tmp4;
#line 319 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(llvm_cbe_tmp5, (((unsigned char *)(&llvm_cbe_y_2e_addr))));
#line 319 "small.cpp"
   // NOWARN;
#line 320 "small.cpp"
  llvm_cbe_tmp6 = *(&llvm_cbe_pxx);
#line 320 "small.cpp"
  llvm_cbe_call8 = _Z9API_CALL1iPv(0u, (((unsigned char *)llvm_cbe_tmp6)));
#line 320 "small.cpp"
   // NOWARN;
#line 322 "small.cpp"
  llvm_cbe_tmp9 = *(&llvm_cbe_glob_2e_addr);
#line 322 "small.cpp"
  llvm_cbe_call11 = _Z9API_CALL1iPv(0u, (((unsigned char *)llvm_cbe_tmp9)));
#line 322 "small.cpp"
   // WARN;
#line 323 "small.cpp"
  llvm_cbe_tmp13 = *((&llvm_cbe_this1->field3));
#line 323 "small.cpp"
  llvm_cbe_call14 = _Z9API_CALL0i(llvm_cbe_tmp13);
#line 323 "small.cpp"
   // NOWARN;
#line 325 "small.cpp"
  llvm_cbe_call16 = _ZN12my_namespace3UES9undef_funEv(llvm_cbe_this1);
#line 325 "small.cpp"
  *(&llvm_cbe_pp) = llvm_cbe_call16;
#line 325 "small.cpp"
   // WARN;
#line 326 "small.cpp"
  llvm_cbe_tmp17 = *(&llvm_cbe_pp);
#line 326 "small.cpp"
  *llvm_cbe_tmp17 = 0u;
#line 326 "small.cpp"
   // WARN;
#line 328 "small.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_glob_2e_addr);
#line 328 "small.cpp"
  *llvm_cbe_tmp18 = 0u;
#line 328 "small.cpp"
   // WARN;
#line 331 "small.cpp"
  llvm_cbe_call21 = _Z9API_CALL1iPv(0u, (((unsigned char *)(&llvm_cbe_mlpi))));
#line 332 "small.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_mlpi);
#line 332 "small.cpp"
  *((&(*llvm_cbe_tmp22))) = 0u;
#line 332 "small.cpp"
   // WARN;
#line 334 "small.cpp"
   // NOWARN;
#line 334 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)((&llvm_cbe_this1->field3))));
#line 334 "small.cpp"
  llvm_cbe_tmp__16 = *(&llvm_cbe_retval);
#line 334 "small.cpp"
  return llvm_cbe_tmp__16;
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
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned int *llvm_cbe_tmp4;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp5;
  unsigned char *llvm_cbe_tmp__17;

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
#line 339 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_stack_i_2e_addr);
#line 339 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 339 "small.cpp"
   // NOWARN;
#line 341 "small.cpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_stack_i_2e_addr);
#line 341 "small.cpp"
  llvm_cbe_tmp3 = *llvm_cbe_tmp2;
#line 341 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_stack_i_2e_addr);
#line 341 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(llvm_cbe_tmp3, (((unsigned char *)llvm_cbe_tmp4)));
#line 341 "small.cpp"
   // NOWARN;
#line 343 "small.cpp"
   // NOWARN;
#line 343 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_stack_i_2e_addr);
#line 343 "small.cpp"
  *(&llvm_cbe_retval) = (((unsigned char *)llvm_cbe_tmp5));
#line 343 "small.cpp"
  llvm_cbe_tmp__17 = *(&llvm_cbe_retval);
#line 343 "small.cpp"
  return llvm_cbe_tmp__17;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES14PrivateMember3EPiiPj(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_glob_i, unsigned int llvm_cbe_y, unsigned int *llvm_cbe_glob) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_glob_i_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_y_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_glob_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp3;
  unsigned int *llvm_cbe_tmp6;
  unsigned char *llvm_cbe_tmp__18;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_glob_i_2e_addr) = llvm_cbe_glob_i;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_y_2e_addr) = llvm_cbe_y;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_glob_2e_addr) = llvm_cbe_glob;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 348 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_glob_i_2e_addr);
#line 348 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 348 "small.cpp"
   // WARN;
#line 350 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(0u, (((unsigned char *)(&llvm_cbe_glob_i_2e_addr))));
#line 350 "small.cpp"
   // WARN;
#line 351 "small.cpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_glob_i_2e_addr);
#line 351 "small.cpp"
  *((&(*llvm_cbe_tmp2))) = 0u;
#line 351 "small.cpp"
   // WARN;
#line 353 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_glob_i_2e_addr);
#line 353 "small.cpp"
  *((&llvm_cbe_this1->field9)) = llvm_cbe_tmp3;
#line 353 "small.cpp"
   // WARN;
#line 354 "small.cpp"
  llvm_cbe_tmp6 = *((&llvm_cbe_this1->field9));
#line 354 "small.cpp"
  *llvm_cbe_tmp6 = 0u;
#line 354 "small.cpp"
   // WARN;
#line 356 "small.cpp"
   // NOWARN;
#line 356 "small.cpp"
  *(&llvm_cbe_retval) = ((unsigned char *)/*NULL*/0);
#line 356 "small.cpp"
  llvm_cbe_tmp__18 = *(&llvm_cbe_retval);
#line 356 "small.cpp"
  return llvm_cbe_tmp__18;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZN12my_namespace3UES14PrivateMember4EPPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int **llvm_cbe_ipppppq) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int **llvm_cbe_ipppppq_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_p1;    /* Address-exposed local */
  unsigned int **llvm_cbe___p2;    /* Address-exposed local */
  unsigned int **llvm_cbe__p2;    /* Address-exposed local */
  unsigned int *llvm_cbe_mpg;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int **llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned int **llvm_cbe_tmp4;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp6;
  unsigned int **llvm_cbe_tmp7;
  unsigned int *llvm_cbe_tmp8;
  unsigned int *llvm_cbe_call11;
  unsigned int *llvm_cbe_tmp12;
  unsigned int **llvm_cbe_tmp13;
  unsigned int *llvm_cbe_tmp14;
  unsigned int *llvm_cbe_call17;
  unsigned int **llvm_cbe_tmp18;
  unsigned int *llvm_cbe_call22;
  unsigned int *llvm_cbe_call24;
  unsigned int *llvm_cbe_tmp26;
  unsigned int **llvm_cbe_tmp29;
  unsigned int *llvm_cbe_tmp30;
  unsigned char *llvm_cbe_tmp__19;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ipppppq_2e_addr) = llvm_cbe_ipppppq;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 277 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_ipppppq_2e_addr);
#line 277 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 277 "small.cpp"
  llvm_cbe_tmp3 = *llvm_cbe_tmp2;
#line 277 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_ipppppq_2e_addr);
#line 277 "small.cpp"
  llvm_cbe_call = _Z10XXXXXXXXXXiPv(llvm_cbe_tmp3, (((unsigned char *)llvm_cbe_tmp4)));
#line 277 "small.cpp"
   // WARN;
#line 279 "small.cpp"
  llvm_cbe_tmp6 = *((&llvm_cbe_this1->field10));
#line 279 "small.cpp"
  *((&(*llvm_cbe_tmp6))) = 0u;
#line 279 "small.cpp"
   // WARN;
#line 282 "small.cpp"
  llvm_cbe_tmp7 = *(&llvm_cbe_ipppppq_2e_addr);
#line 282 "small.cpp"
  llvm_cbe_tmp8 = *llvm_cbe_tmp7;
#line 282 "small.cpp"
  *llvm_cbe_tmp8 = 0u;
#line 282 "small.cpp"
   // WARN;
#line 285 "small.cpp"
  *(&llvm_cbe___p2) = (&llvm_cbe_p1);
#line 287 "small.cpp"
  llvm_cbe_call11 = _Z18API_GET_GLOBAL_MEMv();
#line 287 "small.cpp"
  *(&llvm_cbe_p1) = llvm_cbe_call11;
#line 290 "small.cpp"
  llvm_cbe_tmp12 = *(&llvm_cbe_p1);
#line 290 "small.cpp"
  *llvm_cbe_tmp12 = 0u;
#line 290 "small.cpp"
   // WARN;
#line 291 "small.cpp"
  llvm_cbe_tmp13 = *(&llvm_cbe___p2);
#line 291 "small.cpp"
  llvm_cbe_tmp14 = *llvm_cbe_tmp13;
#line 291 "small.cpp"
  *llvm_cbe_tmp14 = 0u;
#line 291 "small.cpp"
   // WARN;
#line 295 "small.cpp"
  *(&llvm_cbe__p2) = ((&llvm_cbe_this1->field11));
#line 295 "small.cpp"
   // WARN;
#line 297 "small.cpp"
  llvm_cbe_call17 = _Z18API_GET_GLOBAL_MEMv();
#line 297 "small.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe__p2);
#line 297 "small.cpp"
  *llvm_cbe_tmp18 = llvm_cbe_call17;
#line 297 "small.cpp"
   // WARN;
#line 300 "small.cpp"
  *((&llvm_cbe_this1->field13)) = ((&llvm_cbe_this1->field12));
#line 300 "small.cpp"
   // WARN;
#line 303 "small.cpp"
  llvm_cbe_call22 = _Z18API_GET_GLOBAL_MEMv();
#line 303 "small.cpp"
  *(&llvm_cbe_mpg) = llvm_cbe_call22;
#line 304 "small.cpp"
  llvm_cbe_call24 = _ZN12my_namespace6memcpyEPiS0_i((((unsigned int *)((&llvm_cbe_this1->field12)))), (((unsigned int *)(&llvm_cbe_mpg))), 8u);
#line 304 "small.cpp"
   // WARN;
#line 306 "small.cpp"
  llvm_cbe_tmp26 = *((&llvm_cbe_this1->field12));
#line 306 "small.cpp"
  *((&(*llvm_cbe_tmp26))) = 0u;
#line 306 "small.cpp"
   // WARN;
#line 307 "small.cpp"
  llvm_cbe_tmp29 = *((&llvm_cbe_this1->field13));
#line 307 "small.cpp"
  llvm_cbe_tmp30 = *llvm_cbe_tmp29;
#line 307 "small.cpp"
  *((&(*llvm_cbe_tmp30))) = 0u;
#line 307 "small.cpp"
   // WARN;
#line 309 "small.cpp"
  *(&llvm_cbe_retval) = ((unsigned char *)/*NULL*/0);
#line 310 "small.cpp"
  llvm_cbe_tmp__19 = *(&llvm_cbe_retval);
#line 310 "small.cpp"
  return llvm_cbe_tmp__19;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZN12my_namespace3UES8RetTest6EPi(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this, unsigned int *llvm_cbe_pi) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pi_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp__20;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pi_2e_addr) = llvm_cbe_pi;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 361 "small.cpp"
   // NOWARN;
#line 361 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pi_2e_addr);
#line 361 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp;
#line 361 "small.cpp"
  llvm_cbe_tmp__20 = *(&llvm_cbe_retval);
#line 361 "small.cpp"
  return llvm_cbe_tmp__20;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3Log5doLogEPc(unsigned char *llvm_cbe_txt) {
  unsigned char *llvm_cbe_txt_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  unsigned char *llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_txt_2e_addr) = llvm_cbe_txt;
#line 172 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_txt_2e_addr);
#line 172 "small.cpp"
  llvm_cbe_call = printf(((&_OC_str2.array[((signed int )0u)])), llvm_cbe_tmp);
#line 172 "small.cpp"
   // NOWARN;
#line 173 "small.cpp"
  llvm_cbe_tmp1 = *(&llvm_cbe_txt_2e_addr);
#line 173 "small.cpp"
  *llvm_cbe_tmp1 = ((unsigned char )0);
#line 173 "small.cpp"
   // WARN;
#line 175 "small.cpp"
  llvm_cbe_tmp2 = *(&_ZZN12my_namespace3Log5doLogEPcE2kk);
#line 175 "small.cpp"
  *(&_ZZN12my_namespace3Log5doLogEPcE2kk) = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 175 "small.cpp"
   // WARN;
#line 176 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_X3ADDEv(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 218 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field3);
#line 218 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 218 "small.cpp"
  *llvm_cbe_tmp = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 220 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_unnamed14 _ZN12my_namespace3Log12get_priv_funEv(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) {
  struct l_unnamed14 llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed14 llvm_cbe_mfp;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__21;
  struct l_unnamed14 llvm_cbe_tmp__22;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 197 "small.cpp"
  *((&llvm_cbe_mfp.field0)) = ((unsigned long long )(unsigned long)_ZN12my_namespace3Log11my_priv_funEPi);
#line 197 "small.cpp"
  *((&llvm_cbe_mfp.field1)) = 0ull;
#line 197 "small.cpp"
   // WARN;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__21 = memcpy((((unsigned char *)(&llvm_cbe_retval))), (((unsigned char *)(&llvm_cbe_mfp))), 16ull);
#line 198 "small.cpp"
  llvm_cbe_tmp__22 = ((struct __attribute__ ((packed, aligned(1))) {struct l_unnamed14 data; } *)(&llvm_cbe_retval))->data;
#line 198 "small.cpp"
  return llvm_cbe_tmp__22;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZN12my_namespace3Log11my_priv_funEPi(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this, unsigned int *llvm_cbe_pi) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_pi_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp__23;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pi_2e_addr) = llvm_cbe_pi;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 142 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pi_2e_addr);
#line 142 "small.cpp"
  *llvm_cbe_tmp = 0u;
#line 142 "small.cpp"
   // WARN;
#line 145 "small.cpp"
  llvm_cbe_tmp2 = *(&_ZZN12my_namespace3Log11my_priv_funEPiE2xx);
#line 145 "small.cpp"
  *(&_ZZN12my_namespace3Log11my_priv_funEPiE2xx) = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 145 "small.cpp"
   // WARN;
#line 146 "small.cpp"
  *(&llvm_cbe_retval) = ((&llvm_cbe_this1->field5));
#line 146 "small.cpp"
  llvm_cbe_tmp__23 = *(&llvm_cbe_retval);
#line 146 "small.cpp"
  return llvm_cbe_tmp__23;
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
#line 224 "small.cpp"
  *((&llvm_cbe_this1->field3)) = 0u;
#line 225 "small.cpp"
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
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__24;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 229 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field3);
#line 229 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 229 "small.cpp"
  *llvm_cbe_tmp = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 230 "small.cpp"
  llvm_cbe_tmp4 = *((&llvm_cbe_this1->field3));
#line 230 "small.cpp"
  if (((((signed int )(((signed int )llvm_cbe_tmp4) % ((signed int )5u)))) != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 231 "small.cpp"
  llvm_cbe_call = _Z9API_CALL1iPv(0u, (((unsigned char *)((&llvm_cbe_this1->field3)))));
#line 231 "small.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 233 "small.cpp"
  llvm_cbe_tmp6 = *(&llvm_cbe_ev_2e_addr);
#line 233 "small.cpp"
  if ((llvm_cbe_tmp6 == ((struct l_struct_OC_my_namespace_KD__KD_UEC_Event *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 233 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp6)));
#line 233 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
#line 233 "small.cpp"
   // NOWARN;
#line 234 "small.cpp"
  *(&llvm_cbe_retval) = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)/*NULL*/0);
#line 235 "small.cpp"
  llvm_cbe_tmp__24 = *(&llvm_cbe_retval);
#line 235 "small.cpp"
  return llvm_cbe_tmp__24;
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
#line 225 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3UESC2Ev(struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_UES *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp10;
  unsigned int llvm_cbe_tmp11;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3LogC2Ev((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )32ull)])))));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace3UESE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field3)) = 0u;
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace8SubUES_XC1Ev(((&llvm_cbe_this1->field6)));
#line 263 "small.cpp"
  *((&llvm_cbe_this1->field7)) = ((unsigned int *)/*NULL*/0);
#line 263 "small.cpp"
   // WARN;
#line 264 "small.cpp"
  *((&llvm_cbe_this1->field3)) = 0u;
#line 264 "small.cpp"
   // NOWARN;
#line 265 "small.cpp"
  llvm_cbe_tmp10 = *((&llvm_cbe_this1->field7));
#line 265 "small.cpp"
  llvm_cbe_tmp11 = *llvm_cbe_tmp10;
#line 265 "small.cpp"
  *llvm_cbe_tmp10 = (((unsigned int )(((unsigned int )llvm_cbe_tmp11) + ((unsigned int )4294967295u))));
#line 265 "small.cpp"
   // WARN;
#line 266 "small.cpp"
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
#line 125 "small.cpp"
  llvm_cbe_call = printf(((&_OC_str3.array[((signed int )0u)])));
#line 125 "small.cpp"
   // NOWARN;
#line 126 "small.cpp"
  llvm_cbe_tmp = *(&_ZN12my_namespace8CBaseFSM5fubarE);
#line 126 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = (((unsigned int )(((unsigned int )llvm_cbe_tmp) + ((unsigned int )1u))));
#line 126 "small.cpp"
   // WARN;
#line 127 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace3LogC2Ev(struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_Log *llvm_cbe_this1;
  struct l_class_OC_my_namespace_KD__KD_CBaseFSM *llvm_cbe_tmp__25;
  struct l_unnamed14 *llvm_cbe_tmp;
  struct l_unnamed14 *llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__25 = ((struct l_class_OC_my_namespace_KD__KD_CBaseFSM *)llvm_cbe_this1);
#line 180 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field1);
#line 180 "small.cpp"
  *((&llvm_cbe_tmp->field0)) = ((unsigned long long )(unsigned long)_ZN12my_namespace3Log11my_priv_funEPi);
#line 180 "small.cpp"
  *((&llvm_cbe_tmp->field1)) = 0ull;
#line 182 "small.cpp"
  llvm_cbe_tmp2 = (&llvm_cbe_this1->field2);
#line 182 "small.cpp"
  *((&llvm_cbe_tmp2->field0)) = ((unsigned long long )(unsigned long)_ZN12my_namespace3Log15my_undef_fp_funEPi);
#line 182 "small.cpp"
  *((&llvm_cbe_tmp2->field1)) = 0ull;
#line 183 "small.cpp"
  *((&llvm_cbe_this1->field6)) = ((unsigned int *)/*NULL*/0);
#line 184 "small.cpp"
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
#line 214 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12my_namespace8SubUES_XC2Ev(struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this) {
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_my_namespace_KD__KD_SubUES_X *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3FSMC2Ev((((struct l_class_OC_my_namespace_KD__KD_FSM *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  _ZN12my_namespace3LogC2Ev((((struct l_class_OC_my_namespace_KD__KD_Log *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )32ull)])))));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN12my_namespace8SubUES_XE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field3)) = 0u;
#line 214 "small.cpp"
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
#line 113 "small.cpp"
  *(&_ZN12my_namespace8CBaseFSM5fubarE) = 0u;
#line 113 "small.cpp"
   // WARN;
#line 114 "small.cpp"
  return;
}

