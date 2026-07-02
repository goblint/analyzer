// CRAM

/***********************************************************************************
                         C-DAC Tech Workshop : hyPACK-2013
                             October 15-18,2013
  Example     : pthread-demo-datarace.c

  Objective   : Write Pthread code to illustrate Data Race Condition
            and its solution using MUTEX.

  Input       : Nothing.

  Output      : Value of Global variable with and without using Mutex.

  Created     :MAY-2013

  E-mail      : hpcfte@cdac.in

****************************************************************************/

/*
Modifications are made to remove non-standard libary dependencies by Yihao from
VSL of University of Delaware.
 */
extern void abort(void);
#include <assert.h>
void reach_error() {  }
void __VERIFIER_assert(int cond) { if (!(cond)) { ERROR: {reach_error();abort();} } return; } //TODO
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

int myglobal;                                        // declaration of global variable
pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;  // initialization of MUTEX variable

void *thread_function_mutex(void *arg)   // Function which operates on myglobal using mutex
{
    int i,j;
    for ( i=0; i<3; i++ )
    {
        pthread_mutex_lock(&mymutex);
        j=myglobal;
        j=j+1;
        printf("\nIn thread_function_mutex..\t");
  //        fflush(stdout);
        myglobal=j;
        pthread_mutex_unlock(&mymutex);
    }
    return NULL;
}

int main(void)
{
    pthread_t mythread;
    int i;

    printf("\n\t\t---------------------------------------------------------------------------");
    printf("\n\t\t Centre for Development of Advanced Computing (C-DAC)");
    printf("\n\t\t Email : hpcfte@cdac.in");
    printf("\n\t\t---------------------------------------------------------------------------");

    myglobal = 0;

    if ( pthread_create( &mythread, NULL, thread_function_mutex, NULL) ) // calling thread_function_mutex
    {
      exit(-1);
    }
    for ( i=0; i<3; i++)
    {
        pthread_mutex_lock(&mymutex);
        myglobal=myglobal+1;
        pthread_mutex_unlock(&mymutex);
    }
    //      fflush(stdout);
    if ( pthread_join ( mythread, NULL ) )
    {
      exit(-1);
    }

    __VERIFIER_assert(myglobal == 6); // TODO
    exit(0);
}
