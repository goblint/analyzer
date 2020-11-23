// PARAM: --set ana.activated[+] "'var_eq'"
// Minimized from pthread-driver-races/char_generic_nvram_read_nvram_write_nvram.i
// Somehow global whoop_loff_t may not point to NULL in the original, but it may in this one. How???
#include <pthread.h>

extern void abort(void);

extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
extern void __assert_perror_fail (int __errnum, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));

void reach_error() { ((void) sizeof ((0) ? 1 : 0), __extension__ ({ if (0) ; else __assert_fail ("0", "svcomp.h", 24, __extension__ __PRETTY_FUNCTION__); })); }
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}
void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}

typedef long long loff_t;

void read_nvram(loff_t *ppos)
{
 unsigned int i;
 *ppos = i;
 __VERIFIER_assert(*ppos == i);
}
void write_nvram(loff_t *ppos)
{
 unsigned int i;
 *ppos = i;
 __VERIFIER_assert(*ppos == i);
}
loff_t *whoop_loff_t;
void *whoop_wrapper_write_nvram(void* args)
{
 write_nvram(whoop_loff_t);
 return ((void *)0);
}
void *whoop_wrapper_read_nvram(void* args)
{
 read_nvram(whoop_loff_t);
 return ((void *)0);
}
int main(void)
{
 whoop_loff_t = (loff_t *) malloc(sizeof(loff_t));
 pthread_t pthread_t_write_nvram;
 pthread_t pthread_t_read_nvram;
 pthread_create(&pthread_t_write_nvram, ((void *)0), whoop_wrapper_write_nvram, ((void *)0));
 pthread_create(&pthread_t_read_nvram, ((void *)0), whoop_wrapper_read_nvram, ((void *)0));
 pthread_join(pthread_t_write_nvram, ((void *)0));
 pthread_join(pthread_t_read_nvram, ((void *)0));
}
