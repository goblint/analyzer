/******************************************************************************
                            C-DAC Tech Workshop : hyPACK-2013
                             October 15-18,2013

 Example    : pthread-numerical-integration.c

 Objective  : Calculate the Pi Value using simple Integration

 Input          : Number Of Intervals

 Output         : Pi Value computed using simple Integration
              Time Taken for Pi Computation(in Seconds).

 Created    : MAY-2013
 E-mail         : hpcfte@cdac.in

*******************************************************************************/
/*                                                                                                                                                                                   Modifications are made to remove non-standard library depedencies by Yihao from VSL of University of Delaware.                                                                    **/

// #include <assert.h>
#include<pthread.h>


int iCount, num_threads;


/* Thread callback function  */
void myPartOfCalc(int myID)
{

}

/* Main function starts */
int main(int argc, char *argv[])
{
    pthread_t * threads;
    num_threads = __VERIFIER_nondet_int() ;

    iCount = 0;
    while (iCount < num_threads)
    {
        pthread_create(&threads, NULL, myPartOfCalc, NULL);
        iCount++;
    }
    return 0;
}
