// SKIP PARAM: --set ana.activated "['base','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set']" --sets ana.osek.oil 04-cubbyhole.oil --sets ana.osek.tramp 04-defaultAppWorkstation/tpl_os_generated_configuration.h -I 04-defaultAppWorkstation/ -I 04-defaultAppWorkstation/os-minimalheaders/os_machine/posix-libpcl/ -I 04-defaultAppWorkstation/os-minimalheaders/ --sets ana.osek.taskprefix function_of_ --sets ana.osek.isrprefix function_of_

#include <stdio.h>
#include <string.h>
/*#include "tpl_os.h"*/
// #include "tpl_os_generated_configuration.h"

#define _XOPEN_SOURCE 500
#include <unistd.h>

char* cubbyHole = "pong";

/*Autostarted once at system start. Blocks in WaitEvent(...)*/
TASK(ping)
{
    printf("Ping started.\n");
    while(1){
        // WaitEvent(pong_deployed);
        // ClearEvent(pong_deployed);
        GetResource(cubbyHoleMutex);
        cubbyHole = "ping"; //NORACE
        printf("Current state is: %s\n", cubbyHole);
        ReleaseResource(cubbyHoleMutex);
        // SetEvent(pong, ping_deployed);
    }
    TerminateTask();
}

/*Autostarted once at system start. Blocks in WaitEvent(...)*/
TASK(pong)
{
    printf("Pong started.\n");
    while(1){
        // WaitEvent(ping_deployed);
        // ClearEvent(ping_deployed);
        GetResource(cubbyHoleMutex);
        cubbyHole = "pong"; //NORACE
        printf("Current state is: %s\n", cubbyHole);
        ReleaseResource(cubbyHoleMutex);
        // SetEvent(ping, pong_deployed);
    }
}

/* Started once after 10 seconds with high priority. */
TASK(shutdown)
{
    printf("Shutting down...");
    ShutdownOS(E_OK);
}
