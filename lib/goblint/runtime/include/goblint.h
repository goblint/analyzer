void __goblint_check_inner(_Bool exp, char *expstr);
void __goblint_assume_inner(_Bool exp, char *expstr);
void __goblint_assert_inner(_Bool exp, char *expstr);

#define __goblint_check(exp) __goblint_check_inner(exp, #exp)
#define __goblint_assume(exp) __goblint_assume_inner(exp, #exp)
#define __goblint_assert(exp) __goblint_assert_inner(exp, #exp)

void __goblint_assume_join(/* pthread_t thread */); // undeclared argument to avoid pthread.h interfering with Linux kernel headers
void __goblint_globalize(void *ptr);

void __goblint_split_begin(int exp);
void __goblint_split_end(int exp);

void __goblint_bounded(unsigned long long exp);