#include "goblint.h"
#include <pthread.h>

// Empty implementations (instead of asserts) because annotating documentation promises no-op right now.

void __goblint_check(_Bool exp) {

}

void __goblint_assume(_Bool exp) {

}

void __goblint_assert(_Bool exp) {

}


void __goblint_assume_join(pthread_t thread) {

}


void __goblint_split_begin(_Bool exp) {

}

void __goblint_split_end(_Bool exp) {

}

void __goblint_bounded(unsigned long long exp) {

}