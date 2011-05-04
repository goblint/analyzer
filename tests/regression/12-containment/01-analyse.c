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
struct l_class_OC_UECEvent;
struct l_class_OC_UECManagerFSM;
struct l_class_OC_UECProtocolA;
struct l_class_OC_UECProtocolInterface;
struct l_unnamed0;
struct l_unnamed1;
struct l_unnamed2;
struct l_unnamed3;
struct l_unnamed4;
struct l_unnamed5;
struct l_unnamed6;
struct l_unnamed7;

/* Typedefs */
typedef struct l_class_OC_UECEvent l_class_OC_UECEvent;
typedef struct l_class_OC_UECManagerFSM l_class_OC_UECManagerFSM;
typedef struct l_class_OC_UECProtocolA l_class_OC_UECProtocolA;
typedef struct l_class_OC_UECProtocolInterface l_class_OC_UECProtocolInterface;
typedef struct l_unnamed0 l_unnamed0;
typedef struct l_unnamed1 l_unnamed1;
typedef struct l_unnamed2 l_unnamed2;
typedef struct l_unnamed3 l_unnamed3;
typedef struct l_unnamed4 l_unnamed4;
typedef struct l_unnamed5 l_unnamed5;
typedef struct l_unnamed6 l_unnamed6;
typedef struct l_unnamed7 l_unnamed7;

/* Structure contents */
struct l_class_OC_UECEvent {  unsigned int *field0;};

struct l_unnamed3 { unsigned char array[8]; };

struct l_unnamed0 { struct l_class_OC_UECProtocolInterface *array[2]; };

struct l_class_OC_UECManagerFSM {  struct l_unnamed3 field0;  struct l_unnamed0 field1;};

struct l_class_OC_UECProtocolA {  struct l_unnamed3 field0;  struct l_class_OC_UECManagerFSM *field1;  unsigned int field2;};

struct l_class_OC_UECProtocolInterface {  unsigned int  (**field0) ( int, ...);};

struct l_unnamed1 { unsigned char array[16]; };

struct l_unnamed2 {  unsigned char *field0;  unsigned char *field1;};

struct l_unnamed4 { unsigned char array[23]; };

struct l_unnamed5 { unsigned char array[15]; };

struct l_unnamed6 {  unsigned char *field0;  unsigned char *field1;  unsigned char *field2;};

struct l_unnamed7 { unsigned char *array[5]; };


/* External Global Variable Declarations */
extern struct l_unnamed7 _ZTV12UECProtocolA;
extern unsigned char *_ZTVN10__cxxabiv120__si_class_type_infoE;
extern struct l_unnamed5 _ZTS12UECProtocolA;
extern unsigned char *_ZTVN10__cxxabiv117__class_type_infoE;
extern struct l_unnamed6 _ZTI12UECProtocolA;
extern struct l_unnamed7 _ZTV12UECProtocolB;
extern struct l_unnamed5 _ZTS12UECProtocolB;
extern struct l_unnamed6 _ZTI12UECProtocolB;

/* Function Declarations */
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
void _Z5__GWNv(void);
void _Z5__GERv(void);
void _Z5__GNWv(void);
unsigned int _ZN12UECProtocolA12ProcessEventEP8UECEvent(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev);
struct l_class_OC_UECProtocolInterface *_ZN13UECManagerFSM3GetEi(struct l_class_OC_UECManagerFSM *llvm_cbe_this, unsigned int llvm_cbe_i) __ATTRIBUTE_WEAK__;
void _ZdaPv(unsigned char *);
void _ZdlPv(unsigned char *);
unsigned int _ZN12UECProtocolB12ProcessEventEP8UECEvent(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev);
unsigned int main(unsigned int llvm_cbe_argc, unsigned char **llvm_cbe_argv);
void _ZN13UECManagerFSMC1Ev(struct l_class_OC_UECManagerFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN13UECManagerFSM4InitEv(struct l_class_OC_UECManagerFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned char *_Znwm(unsigned long long );
unsigned char *_Znam(unsigned long long );
unsigned int _ZN13UECManagerFSM12ProcessEventEP8UECEvent(struct l_class_OC_UECManagerFSM *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolA4InitEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolA8ShutDownEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolB4InitEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolB8ShutDownEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolAC1EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolBC1EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) __ATTRIBUTE_WEAK__;
void _ZN12UECProtocolBC2EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) __ATTRIBUTE_WEAK__;
void _ZN20UECProtocolInterfaceC2Ev(struct l_class_OC_UECProtocolInterface *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void __cxa_pure_virtual(void);
void _ZN12UECProtocolAC2EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) __ATTRIBUTE_WEAK__;
void _ZN13UECManagerFSMC2Ev(struct l_class_OC_UECManagerFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN13UECManagerFSM8ShutDownEv(struct l_class_OC_UECManagerFSM *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void abort(void);


/* Global Variable Declarations */
extern struct l_unnamed7 _ZTV12UECProtocolA;
extern struct l_unnamed5 _ZTS12UECProtocolA;
extern struct l_unnamed4 _ZTS20UECProtocolInterface __ATTRIBUTE_WEAK__;
extern struct l_unnamed2 _ZTI20UECProtocolInterface __ATTRIBUTE_WEAK__;
extern struct l_unnamed6 _ZTI12UECProtocolA;
extern struct l_unnamed7 _ZTV12UECProtocolB;
extern struct l_unnamed5 _ZTS12UECProtocolB;
extern struct l_unnamed6 _ZTI12UECProtocolB;
extern struct l_unnamed7 _ZTV20UECProtocolInterface __ATTRIBUTE_WEAK__;
extern struct l_unnamed7 _ZTV13UECManagerFSM __ATTRIBUTE_WEAK__;
extern struct l_unnamed1 _ZTS13UECManagerFSM __ATTRIBUTE_WEAK__;
extern struct l_unnamed6 _ZTI13UECManagerFSM __ATTRIBUTE_WEAK__;


/* Global Variable Definitions and Initialization */
struct l_unnamed7 _ZTV12UECProtocolA = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTI12UECProtocolA)), ((unsigned char *)_ZN12UECProtocolA4InitEv), ((unsigned char *)_ZN12UECProtocolA8ShutDownEv), ((unsigned char *)_ZN12UECProtocolA12ProcessEventEP8UECEvent) } };
struct l_unnamed5 _ZTS12UECProtocolA = { "12UECProtocolA" };
struct l_unnamed4 _ZTS20UECProtocolInterface __ATTRIBUTE_WEAK__ = { "20UECProtocolInterface" };
struct l_unnamed2 _ZTI20UECProtocolInterface __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv117__class_type_infoE)[((signed long long )2ull)]))), ((&_ZTS20UECProtocolInterface.array[((signed int )0u)])) };
struct l_unnamed6 _ZTI12UECProtocolA = { ((unsigned char *)((&(&_ZTVN10__cxxabiv120__si_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTS12UECProtocolA.array[((signed int )0u)])), ((unsigned char *)(&_ZTI20UECProtocolInterface)) };
struct l_unnamed7 _ZTV12UECProtocolB = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTI12UECProtocolB)), ((unsigned char *)_ZN12UECProtocolB4InitEv), ((unsigned char *)_ZN12UECProtocolB8ShutDownEv), ((unsigned char *)_ZN12UECProtocolB12ProcessEventEP8UECEvent) } };
struct l_unnamed5 _ZTS12UECProtocolB = { "12UECProtocolB" };
struct l_unnamed6 _ZTI12UECProtocolB = { ((unsigned char *)((&(&_ZTVN10__cxxabiv120__si_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTS12UECProtocolB.array[((signed int )0u)])), ((unsigned char *)(&_ZTI20UECProtocolInterface)) };
struct l_unnamed7 _ZTV20UECProtocolInterface __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTI20UECProtocolInterface)), ((unsigned char *)__cxa_pure_virtual), ((unsigned char *)__cxa_pure_virtual), ((unsigned char *)__cxa_pure_virtual) } };
struct l_unnamed7 _ZTV13UECManagerFSM __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)(&_ZTI13UECManagerFSM)), ((unsigned char *)_ZN13UECManagerFSM4InitEv), ((unsigned char *)_ZN13UECManagerFSM8ShutDownEv), ((unsigned char *)_ZN13UECManagerFSM12ProcessEventEP8UECEvent) } };
struct l_unnamed1 _ZTS13UECManagerFSM __ATTRIBUTE_WEAK__ = { "13UECManagerFSM" };
struct l_unnamed6 _ZTI13UECManagerFSM __ATTRIBUTE_WEAK__ = { ((unsigned char *)((&(&_ZTVN10__cxxabiv120__si_class_type_infoE)[((signed long long )2ull)]))), ((&_ZTS13UECManagerFSM.array[((signed int )0u)])), ((unsigned char *)(&_ZTI20UECProtocolInterface)) };


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
unsigned int _ZN12UECProtocolA12ProcessEventEP8UECEvent(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECEvent *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  struct l_class_OC_UECManagerFSM *llvm_cbe_tmp2;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_call;
  unsigned int  (**llvm_cbe_tmp__1) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *);
  unsigned int  (*llvm_cbe_tmp__2) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *);
  struct l_class_OC_UECEvent *llvm_cbe_tmp3;
  unsigned int llvm_cbe_call4;
  struct l_class_OC_UECEvent *llvm_cbe_tmp5;
  unsigned int *llvm_cbe_tmp7;
  struct l_class_OC_UECEvent *llvm_cbe_tmp8;
  unsigned int *llvm_cbe_tmp12;
  unsigned int llvm_cbe_tmp13;
  unsigned int llvm_cbe_tmp__3;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 50 "small.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field1));
#line 50 "small.cpp"
  llvm_cbe_call = _ZN13UECManagerFSM3GetEi(llvm_cbe_tmp2, 1u);
#line 50 "small.cpp"
  llvm_cbe_tmp__1 = *(((unsigned int  (***) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *))llvm_cbe_call));
#line 50 "small.cpp"
  llvm_cbe_tmp__2 = *((&llvm_cbe_tmp__1[((signed long long )2ull)]));
#line 50 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_ev_2e_addr);
#line 50 "small.cpp"
  llvm_cbe_call4 = llvm_cbe_tmp__2(llvm_cbe_call, llvm_cbe_tmp3);
#line 52 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_ev_2e_addr);
#line 52 "small.cpp"
  llvm_cbe_tmp7 = *((&llvm_cbe_tmp5->field0));
#line 52 "small.cpp"
  if ((llvm_cbe_tmp7 == ((unsigned int *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 52 "small.cpp"
  _ZdaPv((((unsigned char *)llvm_cbe_tmp7)));
#line 52 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
#line 53 "small.cpp"
  llvm_cbe_tmp8 = *(&llvm_cbe_ev_2e_addr);
#line 53 "small.cpp"
  if ((llvm_cbe_tmp8 == ((struct l_class_OC_UECEvent *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end11;  } else {    goto llvm_cbe_delete_2e_notnull10;  }


llvm_cbe_delete_2e_notnull10:
#line 53 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp8)));
#line 53 "small.cpp"
  goto llvm_cbe_delete_2e_end11;

llvm_cbe_delete_2e_end11:
#line 55 "small.cpp"
  llvm_cbe_tmp12 = (&llvm_cbe_this1->field2);
#line 55 "small.cpp"
  llvm_cbe_tmp13 = *llvm_cbe_tmp12;
#line 55 "small.cpp"
  *llvm_cbe_tmp12 = (((unsigned int )(((unsigned int )llvm_cbe_tmp13) + ((unsigned int )1u))));
#line 55 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp13;
#line 56 "small.cpp"
  llvm_cbe_tmp__3 = *(&llvm_cbe_retval);
#line 56 "small.cpp"
  return llvm_cbe_tmp__3;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_UECProtocolInterface *_ZN13UECManagerFSM3GetEi(struct l_class_OC_UECManagerFSM *llvm_cbe_this, unsigned int llvm_cbe_i) {
  struct l_class_OC_UECProtocolInterface *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_i_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_tmp3;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_tmp__4;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 42 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
#line 42 "small.cpp"
  llvm_cbe_tmp3 = *((&((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp)))]));
#line 42 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 43 "small.cpp"
  llvm_cbe_tmp__4 = *(&llvm_cbe_retval);
#line 43 "small.cpp"
  return llvm_cbe_tmp__4;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN12UECProtocolB12ProcessEventEP8UECEvent(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECEvent *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__5;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 60 "small.cpp"
  llvm_cbe_tmp = (&llvm_cbe_this1->field2);
#line 60 "small.cpp"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 60 "small.cpp"
  *llvm_cbe_tmp = (((unsigned int )(((unsigned int )llvm_cbe_tmp2) + ((unsigned int )1u))));
#line 60 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 61 "small.cpp"
  llvm_cbe_tmp__5 = *(&llvm_cbe_retval);
#line 61 "small.cpp"
  return llvm_cbe_tmp__5;
}


void _ZN13UECManagerFSMC1Ev(struct l_class_OC_UECManagerFSM *llvm_cbe_this);

#line 0 "LLVM INTERNAL"
unsigned int main(unsigned int llvm_cbe_argc, unsigned char **llvm_cbe_argv) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe_argc_2e_addr;    /* Address-exposed local */
  unsigned char **llvm_cbe_argv_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM llvm_cbe_m;    /* Address-exposed local */
  struct l_class_OC_UECEvent *llvm_cbe_pev;    /* Address-exposed local */
  unsigned char *llvm_cbe_call;
  unsigned char *llvm_cbe_call1;
  struct l_class_OC_UECEvent *llvm_cbe_tmp;
  struct l_class_OC_UECEvent *llvm_cbe_tmp3;
  unsigned int llvm_cbe_call4;
  unsigned int llvm_cbe_tmp__6;

  CODE_FOR_MAIN();
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_argc_2e_addr) = llvm_cbe_argc;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_argv_2e_addr) = llvm_cbe_argv;
#line 65 "small.cpp"
  _ZN13UECManagerFSMC1Ev((&llvm_cbe_m));
#line 66 "small.cpp"
  _ZN13UECManagerFSM4InitEv((&llvm_cbe_m));
#line 68 "small.cpp"
  llvm_cbe_call = _Znwm(8ull);
#line 68 "small.cpp"
  *(&llvm_cbe_pev) = (((struct l_class_OC_UECEvent *)llvm_cbe_call));
#line 69 "small.cpp"
  llvm_cbe_call1 = _Znam(40ull);
#line 69 "small.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_pev);
#line 69 "small.cpp"
  *((&llvm_cbe_tmp->field0)) = (((unsigned int *)llvm_cbe_call1));
#line 71 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_pev);
#line 71 "small.cpp"
  llvm_cbe_call4 = _ZN13UECManagerFSM12ProcessEventEP8UECEvent((&llvm_cbe_m), llvm_cbe_tmp3);
#line 71 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_call4;
#line 72 "small.cpp"
  llvm_cbe_tmp__6 = *(&llvm_cbe_retval);
#line 72 "small.cpp"
  return llvm_cbe_tmp__6;
}


#line 0 "LLVM INTERNAL"
void _ZN13UECManagerFSMC1Ev(struct l_class_OC_UECManagerFSM *llvm_cbe_this) {
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN13UECManagerFSMC2Ev(llvm_cbe_this1);
#line 10 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN13UECManagerFSM4InitEv(struct l_class_OC_UECManagerFSM *llvm_cbe_this) {
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_UECProtocolA *llvm_cbe_tmp__7;
  unsigned char *llvm_cbe_call2;
  struct l_class_OC_UECProtocolA *llvm_cbe_tmp__8;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 33 "small.cpp"
  llvm_cbe_call = _Znwm(24ull);
#line 33 "small.cpp"
  llvm_cbe_tmp__7 = ((struct l_class_OC_UECProtocolA *)llvm_cbe_call);
#line 33 "small.cpp"
  _ZN12UECProtocolAC1EP13UECManagerFSM(llvm_cbe_tmp__7, llvm_cbe_this1);
#line 33 "small.cpp"
  *((&(*((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)]))))) = (((struct l_class_OC_UECProtocolInterface *)llvm_cbe_tmp__7));
#line 34 "small.cpp"
  llvm_cbe_call2 = _Znwm(24ull);
#line 34 "small.cpp"
  llvm_cbe_tmp__8 = ((struct l_class_OC_UECProtocolA *)llvm_cbe_call2);
#line 34 "small.cpp"
  _ZN12UECProtocolBC1EP13UECManagerFSM(llvm_cbe_tmp__8, llvm_cbe_this1);
#line 34 "small.cpp"
  *((&((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)]))[((signed long long )1ull)])) = (((struct l_class_OC_UECProtocolInterface *)llvm_cbe_tmp__8));
#line 38 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN13UECManagerFSM12ProcessEventEP8UECEvent(struct l_class_OC_UECManagerFSM *llvm_cbe_this, struct l_class_OC_UECEvent *llvm_cbe_ev) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECEvent *llvm_cbe_ev_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_r;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_tmp2;
  unsigned int  (**llvm_cbe_tmp__9) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *);
  unsigned int  (*llvm_cbe_tmp__10) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *);
  struct l_class_OC_UECEvent *llvm_cbe_tmp3;
  unsigned int llvm_cbe_call;
  struct l_class_OC_UECEvent *llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp5;
  unsigned int llvm_cbe_tmp__11;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_ev_2e_addr) = llvm_cbe_ev;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 25 "small.cpp"
  llvm_cbe_tmp2 = *((&(*((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)])))));
#line 25 "small.cpp"
  llvm_cbe_tmp__9 = *(((unsigned int  (***) (struct l_class_OC_UECProtocolInterface *, struct l_class_OC_UECEvent *))llvm_cbe_tmp2));
#line 25 "small.cpp"
  llvm_cbe_tmp__10 = *((&llvm_cbe_tmp__9[((signed long long )2ull)]));
#line 25 "small.cpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_ev_2e_addr);
#line 25 "small.cpp"
  llvm_cbe_call = llvm_cbe_tmp__10(llvm_cbe_tmp2, llvm_cbe_tmp3);
#line 25 "small.cpp"
  *(&llvm_cbe_r) = llvm_cbe_call;
#line 27 "small.cpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_ev_2e_addr);
#line 27 "small.cpp"
  if ((llvm_cbe_tmp4 == ((struct l_class_OC_UECEvent *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 27 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp4)));
#line 27 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
#line 28 "small.cpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_r);
#line 28 "small.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp5;
#line 29 "small.cpp"
  llvm_cbe_tmp__11 = *(&llvm_cbe_retval);
#line 29 "small.cpp"
  return llvm_cbe_tmp__11;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolA4InitEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 40 "small.h"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 41 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolA8ShutDownEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 45 "small.h"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 46 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolB4InitEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 64 "small.h"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 65 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolB8ShutDownEv(struct l_class_OC_UECProtocolA *llvm_cbe_this) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 69 "small.h"
  *((&llvm_cbe_this1->field2)) = 0u;
#line 70 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolAC1EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_man_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  struct l_class_OC_UECManagerFSM *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_man_2e_addr) = llvm_cbe_man;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_man_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12UECProtocolAC2EP13UECManagerFSM(llvm_cbe_this1, llvm_cbe_tmp);
#line 26 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolBC1EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_man_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  struct l_class_OC_UECManagerFSM *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_man_2e_addr) = llvm_cbe_man;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_man_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN12UECProtocolBC2EP13UECManagerFSM(llvm_cbe_this1, llvm_cbe_tmp);
#line 60 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolBC2EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_man_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  struct l_class_OC_UECManagerFSM *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_man_2e_addr) = llvm_cbe_man;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN20UECProtocolInterfaceC2Ev((((struct l_class_OC_UECProtocolInterface *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV12UECProtocolB.array[((signed long long )2ull)]));
#line 59 "small.h"
  llvm_cbe_tmp = *(&llvm_cbe_man_2e_addr);
#line 59 "small.h"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp;
#line 60 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN20UECProtocolInterfaceC2Ev(struct l_class_OC_UECProtocolInterface *llvm_cbe_this) {
  struct l_class_OC_UECProtocolInterface *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolInterface *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV20UECProtocolInterface.array[((signed long long )2ull)]));
#line 7 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN12UECProtocolAC2EP13UECManagerFSM(struct l_class_OC_UECProtocolA *llvm_cbe_this, struct l_class_OC_UECManagerFSM *llvm_cbe_man) {
  struct l_class_OC_UECProtocolA *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_man_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECProtocolA *llvm_cbe_this1;
  struct l_class_OC_UECManagerFSM *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_man_2e_addr) = llvm_cbe_man;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN20UECProtocolInterfaceC2Ev((((struct l_class_OC_UECProtocolInterface *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV12UECProtocolA.array[((signed long long )2ull)]));
#line 25 "small.h"
  llvm_cbe_tmp = *(&llvm_cbe_man_2e_addr);
#line 25 "small.h"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp;
#line 26 "small.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN13UECManagerFSMC2Ev(struct l_class_OC_UECManagerFSM *llvm_cbe_this) {
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN20UECProtocolInterfaceC2Ev((((struct l_class_OC_UECProtocolInterface *)llvm_cbe_this1)));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV13UECManagerFSM.array[((signed long long )2ull)]));
#line 10 "small.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN13UECManagerFSM8ShutDownEv(struct l_class_OC_UECManagerFSM *llvm_cbe_this) {
  struct l_class_OC_UECManagerFSM *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_UECManagerFSM *llvm_cbe_this1;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_tmp2;
  struct l_class_OC_UECProtocolInterface *llvm_cbe_tmp6;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 19 "small.cpp"
  llvm_cbe_tmp2 = *((&(*((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)])))));
#line 19 "small.cpp"
  if ((llvm_cbe_tmp2 == ((struct l_class_OC_UECProtocolInterface *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 19 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp2)));
#line 19 "small.cpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end:
#line 20 "small.cpp"
  llvm_cbe_tmp6 = *((&((&(*((&llvm_cbe_this1->field1))).array[((signed int )0u)]))[((signed long long )1ull)]));
#line 20 "small.cpp"
  if ((llvm_cbe_tmp6 == ((struct l_class_OC_UECProtocolInterface *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end9;  } else {    goto llvm_cbe_delete_2e_notnull8;  }


llvm_cbe_delete_2e_notnull8:
#line 20 "small.cpp"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp6)));
#line 21 "small.cpp"
  return;

llvm_cbe_delete_2e_end9:
#line 21 "small.cpp"
  return;
}

