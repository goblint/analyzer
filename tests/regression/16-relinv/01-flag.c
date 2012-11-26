//PARAM: --oil 01-flag.oil -I os/ -I 01-defaultAppWorkstation/ -I 01-defaultAppWorkstation/os/os_machine/posix-libpcl/ --tramp 01-defaultAppWorkstation/tpl_os_generated_configuration.h --osektaskprefix function_of_ --osekisrprefix function_of_

#include <stdio.h>
#include <string.h>
#include "tpl_os_generated_configuration.h"

#define _XOPEN_SOURCE 500
#define E_OK 0
#include <unistd.h>

int cs = 0;

typedef struct{
	int x;
	int y;
}point;

point p = {0, 0};

ISR(PosixSignal_USR2)
{
   if(cs == 0){
        p.x =1;
        p.y =1;
	}
}

ISR(printer)
{
       cs = 1;
       printf("%i, %i\n", p.x, p.y);
       p.x = 0;
       p.y = 0;
       cs = 0;
//        usleep(1000); 
}

TASK(shutdown)
{
    printf("Shutting down...");
    ShutdownOS(E_OK);
    TerminateTask();
}
