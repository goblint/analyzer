// PARAM: --set ana.malloc.unique_address_count 2

// Strong updates are not possible in thread because it is not unique.

#include <goblint.h>
#include <stdlib.h>
#include <pthread.h>

void *thread(void *v)
{
    int *x = malloc(sizeof(int));
    int *y = malloc(sizeof(int));

    *x = 0;
    *y = 1;

    *x = 2;
    *y = 3;

    __goblint_check(*x == 2); // UNKNOWN!
    __goblint_check(*y == 3); // UNKNOWN!
}

int main(int argc, char **argv)
{
	pthread_t tids[argc];

    for (int i = 0; i < argc; i++)
        pthread_create(&tids[i], NULL, thread, NULL);

    for (int i = 0; i < argc; i++)
        pthread_join(tids[i], NULL);
}
