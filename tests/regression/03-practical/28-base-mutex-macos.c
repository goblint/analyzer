// Intentionally no #include <pthread.h>, because we want to imitate/debug MacOS construction on anything.

#define __PTHREAD_MUTEX_SIZE__		56

struct _opaque_pthread_mutex_t {
	long __sig;
	char __opaque[__PTHREAD_MUTEX_SIZE__];
};

typedef struct _opaque_pthread_mutex_t __darwin_pthread_mutex_t;
typedef __darwin_pthread_mutex_t pthread_mutex_t;

#define _PTHREAD_MUTEX_SIG_init		0x32AAABA7
#define PTHREAD_MUTEX_INITIALIZER {_PTHREAD_MUTEX_SIG_init, {0}}

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

int main() {
  return 0;
}
