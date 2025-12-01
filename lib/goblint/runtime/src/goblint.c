#include "goblint.h"
#include <pthread.h>

// Empty implementations (instead of asserts) because annotating documentation promises no-op right now.

void __goblint_check_inner(_Bool exp, char *expstr) {

}

void __goblint_assume_inner(_Bool exp, char *expstr) {

}

void __goblint_assert_inner(_Bool exp, char *expstr) {

}


void __goblint_assume_join(pthread_t thread) {

}


void __goblint_split_begin(int exp) {

}

void __goblint_split_end(int exp) {

}

void __goblint_bounded(unsigned long long exp) {

}