extern void __VERIFIER_error() __attribute__ ((__noreturn__));

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

static int a = 0;
static int b = 0;

pthread_mutex_t mut_lock = PTHREAD_MUTEX_INITIALIZER;

void *iSet_1(void *param);
void *iSet_2(void *param);
void *iCheck_1(void *param);
void *iCheck_2(void *param);
void set();
int check();

int main(int argc, char *argv[]) {
    int i, err;

    pthread_t t1;
    pthread_t t2;
    pthread_t t3;
    pthread_t t4;

    if (0 != (err = pthread_create(&t1, NULL, &iSet_1, NULL))) {
        fprintf(stderr, "Error [%d] found creating set thread.\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_create(&t2, NULL, &iSet_2, NULL))) {
        fprintf(stderr, "Error [%d] found creating set thread.\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_create(&t3, NULL, &iCheck_1,
                                    NULL))) {
        fprintf(stderr, "Error [%d] found creating check thread.\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_create(&t4, NULL, &iCheck_2,
                                    NULL))) {
        fprintf(stderr, "Error [%d] found creating check thread.\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_join(t1, NULL))) {
        fprintf(stderr, "pthread join error: %d\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_join(t2, NULL))) {
        fprintf(stderr, "pthread join error: %d\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_join(t3, NULL))) {
        fprintf(stderr, "pthread join error: %d\n", err);
        exit(-1);
    }

    if (0 != (err = pthread_join(t4, NULL))) {
        fprintf(stderr, "pthread join error: %d\n", err);
        exit(-1);
    }

    return 0;
}
        
void *iSet_1(void *param) {
    pthread_mutex_lock(&mut_lock);
    a = 1;
    b = -1;
    pthread_mutex_unlock(&mut_lock);
    return NULL;
}

void *iSet_2(void *param) {
    pthread_mutex_lock(&mut_lock);
    a = 1;
    b = -1;
    pthread_mutex_unlock(&mut_lock);
    return NULL;
}

void *iCheck_1(void *param) {
    pthread_mutex_lock(&mut_lock);
    if (! (a + b == 0)) {
        fprintf(stderr, "Bug found!\n");
    	ERROR: __VERIFIER_error();
    	goto ERROR;
    }
    pthread_mutex_unlock(&mut_lock);

    return NULL;
}

void *iCheck_2(void *param) {
    pthread_mutex_lock(&mut_lock);
    if (! (a + b == 0)) {
        fprintf(stderr, "Bug found!\n");
    	ERROR: __VERIFIER_error();
    	goto ERROR;
    }
    pthread_mutex_unlock(&mut_lock);

    return NULL;
}