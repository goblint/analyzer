#ifndef _PTHREAD_H
#define _PTHREAD_H  1

#define PTHREAD_MUTEX_INITIALIZER 0

#ifndef _BITS_PTHREADTYPES_H
#define _BITS_PTHREADTYPES_H 

#define NULL (void *) 0
typedef int pthread_t;
typedef int __attribute__((mutex)) pthread_mutex_t;

#endif
#endif
