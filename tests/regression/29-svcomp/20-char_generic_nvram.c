// PARAM: --set ana.activated[+] "'var_eq'"
// Minimized from pthread-driver-races/char_generic_nvram_read_nvram_write_nvram.i
#include <pthread.h>
#include <stdlib.h>
#include <goblint.h>

typedef long long loff_t;

void read_nvram(loff_t *ppos)
{
 unsigned int i;
 *ppos = i;
 __goblint_check(*ppos == i); // UNKNOWN!
}
void write_nvram(loff_t *ppos)
{
 unsigned int i;
 *ppos = i;
 __goblint_check(*ppos == i); // UNKNOWN!
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
