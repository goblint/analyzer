void __goblint_check(_Bool exp);
void __goblint_assume(_Bool exp);
void __goblint_assert(_Bool exp);

void __goblint_assume_join(/* pthread_t thread */); // undeclared argument to avoid pthread.h interfering with Linux kernel headers
void __goblint_globalize(void *ptr);

void __goblint_split_begin(int exp);
void __goblint_split_end(int exp);

void __goblint_bounded(unsigned long long exp);