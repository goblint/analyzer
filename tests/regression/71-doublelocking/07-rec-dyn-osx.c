// PARAM: --set ana.activated[+] 'maylocks' --set ana.activated[+] 'pthreadMutexType'
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;
typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
typedef int __darwin_ct_rune_t;


struct __darwin_pthread_handler_rec {
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};

struct _opaque_pthread_attr_t {
 long __sig;
 char __opaque[56];
};

struct _opaque_pthread_mutex_t {
 long __sig;
 char __opaque[56];
};

struct _opaque_pthread_mutexattr_t {
 long __sig;
 char __opaque[8];
};

struct _opaque_pthread_t {
 long __sig;
 struct __darwin_pthread_handler_rec *__cleanup_stack;
 char __opaque[8176];
};

typedef struct _opaque_pthread_attr_t __darwin_pthread_attr_t;
typedef struct _opaque_pthread_mutex_t __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_t *__darwin_pthread_t;

typedef __darwin_pthread_attr_t pthread_attr_t;
typedef __darwin_pthread_mutex_t pthread_mutex_t;
typedef __darwin_pthread_mutexattr_t pthread_mutexattr_t;
typedef __darwin_pthread_t pthread_t;

int pthread_create(pthread_t _Nullable restrict,
  const pthread_attr_t * _Nullable restrict,
  void * ,
  void *);

int pthread_join(pthread_t , void *);
int pthread_mutex_init(pthread_mutex_t * restrict, const pthread_mutexattr_t * _Nullable restrict);
int pthread_mutex_lock(pthread_mutex_t *);
int pthread_mutex_unlock(pthread_mutex_t *);
int pthread_mutexattr_destroy(pthread_mutexattr_t *);
int pthread_mutexattr_init(pthread_mutexattr_t *);
int pthread_mutexattr_settype(pthread_mutexattr_t *, int);


int g;

void* f1(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;

    pthread_mutex_lock(mut);
    pthread_mutex_lock(mut);
    pthread_mutex_unlock(mut);
    pthread_mutex_unlock(mut);
    return ((void *)0);
}


int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_mutex_t mut;

    pthread_mutexattr_t attr;
    pthread_mutexattr_settype(&attr, 2);
    pthread_mutex_init(&mut, &attr);


    pthread_create(&t1,((void *)0),f1,&mut);


    pthread_mutex_lock(&mut);
    pthread_mutex_lock(&mut);
    pthread_mutex_unlock(&mut);
    pthread_mutex_unlock(&mut);

    pthread_join(t1, ((void *)0));


    return 0;
}
