// SKIP PARAM: --sets ana.activated[+] extract_arinc --sets ana.activated[+] thread --sets solver slr3 --disable ana.arinc.export

typedef char * SEMAPHORE_NAME_TYPE;
typedef int    SEMAPHORE_ID_TYPE;
/* typedef int    RETURN_CODE_TYPE; */
enum T13 {
    NO_ERROR = 0,   // request valid and operation performed
    NO_ACTION = 1,  // status of system unaffected by request
    NOT_AVAILABLE = 2,  // resource required by request unavailable
    INVALID_PARAM = 3,  // invalid parameter specified in request
    INVALID_CONFIG = 4, // parameter incompatible with configuration
    INVALID_MODE = 5,   // request incompatible with current mode
    TIMED_OUT = 6   // time-out tied up with request has expired
} ;
typedef enum T13 RETURN_CODE_TYPE;
typedef int    SEMAPHORE_VALUE_TYPE;
typedef void * QUEUING_DISCIPLINE_TYPE;
/* typedef int    SYSTEM_TIME_TYPE; // in monit.c it is defined as unsigned long, PDF says signed 64bit */
typedef long   SYSTEM_TIME_TYPE;

extern void LAP_Se_GetSemaphoreId(SEMAPHORE_NAME_TYPE, SEMAPHORE_ID_TYPE*, RETURN_CODE_TYPE*);
extern void LAP_Se_CreateSemaphore(SEMAPHORE_NAME_TYPE,SEMAPHORE_VALUE_TYPE,SEMAPHORE_VALUE_TYPE,QUEUING_DISCIPLINE_TYPE,SEMAPHORE_ID_TYPE*,RETURN_CODE_TYPE*);
extern void LAP_Se_WaitSemaphore(SEMAPHORE_ID_TYPE,SYSTEM_TIME_TYPE,RETURN_CODE_TYPE*);
extern void LAP_Se_SignalSemaphore(SEMAPHORE_ID_TYPE,RETURN_CODE_TYPE*);

typedef void * PROCESS_NAME_TYPE;
typedef void * SYSTEM_ADDRESS_TYPE;
typedef long   STACK_SIZE_TYPE;
typedef long   PRIORITY_TYPE;
typedef int    DEADLINE_TYPE;

typedef struct {
  PROCESS_NAME_TYPE      NAME;
  SYSTEM_ADDRESS_TYPE    ENTRY_POINT;
  STACK_SIZE_TYPE        STACK_SIZE;
  PRIORITY_TYPE          BASE_PRIORITY;
  SYSTEM_TIME_TYPE       PERIOD;
  SYSTEM_TIME_TYPE       TIME_CAPACITY;
  DEADLINE_TYPE          DEADLINE;
}                        PROCESS_ATTRIBUTE_TYPE;

typedef int PROCESS_ID_TYPE;

extern void LAP_Se_CreateProcess(PROCESS_ATTRIBUTE_TYPE*, PROCESS_ID_TYPE*, RETURN_CODE_TYPE*);

extern void LAP_Se_GetMyId(PROCESS_ID_TYPE*, RETURN_CODE_TYPE*);
extern void LAP_Se_Start(PROCESS_ID_TYPE, RETURN_CODE_TYPE*);

typedef
   enum {
        IDLE       = 0,
        COLD_START = 1,
        WARM_START = 2,
        NORMAL     = 3
   } OPERATING_MODE_TYPE;

extern void LAP_Se_SetPartitionMode (
       /*in */ OPERATING_MODE_TYPE OPERATING_MODE,
       /*out*/ RETURN_CODE_TYPE    *RETURN_CODE );
extern void LAP_Se_PeriodicWait (RETURN_CODE_TYPE *RETURN_CODE);

typedef char T1;
char * F59(T1* destination, const T1* source ); // strcpy
// -----------------------

int g,g2;
SEMAPHORE_ID_TYPE sem_id;

int bg;
void foo2() {
  RETURN_CODE_TYPE r;
  LAP_Se_PeriodicWait(&r);
}
void foo(int b) {
  RETURN_CODE_TYPE r;
  if(b) foo2();
  // if(b) LAP_Se_PeriodicWait(&r);
  PROCESS_ID_TYPE pid;
  LAP_Se_GetMyId(&pid, &r);
  LAP_Se_Start(pid, &r);
  // if(bg) LAP_Se_PeriodicWait(&r);
}

void P1(void){
  // foo(0);
  // foo(1);
  RETURN_CODE_TYPE r;
  while (r){
    LAP_Se_WaitSemaphore(sem_id,600,&r);
    g = g + 1; // NOWARN!
    LAP_Se_SignalSemaphore(sem_id,&r);
  }
  LAP_Se_PeriodicWait(&r);
  LAP_Se_PeriodicWait(&r);
  return;
}

void P2(void){
  // foo(0);
  // foo(1);
  // here we only get 1 context for foo without bg b/c P2 may run arbitrarily
  // foo(0); bg = 1; foo(0);
  // different contexts for different arguments work fine:
  // foo(0); foo(1);
  RETURN_CODE_TYPE r;
  while (1){
    LAP_Se_WaitSemaphore(sem_id,600,&r);
    if(r == NO_ERROR)
        LAP_Se_PeriodicWait(&r);
    g = g - 1;    // NOWARN!
    g2 = g2 + 1;  // RACE!
    LAP_Se_SignalSemaphore(sem_id,&r);
  }
  return;
}

int main(){
  // here we get 2 contexts for foo with bg=0 and bg=1:
  // foo(0); bg = 1; foo(0);
  RETURN_CODE_TYPE r;
  PROCESS_ID_TYPE pi1, pi2;
  SEMAPHORE_ID_TYPE sem_id_local;
  PROCESS_ATTRIBUTE_TYPE p1, p2;
  LAP_Se_CreateSemaphore("my_mutex",1,1,0,&sem_id_local,&r);
  LAP_Se_GetSemaphoreId("my_mutex",&sem_id,&r);
  F59(p1.NAME, (T1 *)"proc1");
  /*p1.NAME = "proc1";*/
  p1.ENTRY_POINT = (void *) &P1;
  p1.BASE_PRIORITY = 10;
  p1.PERIOD = 600;
  p1.TIME_CAPACITY = 600;
  p2.NAME = "proc2";
  p2.ENTRY_POINT = (void *) &P2;
  p2.BASE_PRIORITY = 10;
  p2.PERIOD = 600;
  p2.TIME_CAPACITY = 600;
  LAP_Se_CreateProcess(&p1,&pi1,&r);
  LAP_Se_CreateProcess(&p2,&pi2,&r);
  LAP_Se_Start(pi1,&r);
  LAP_Se_Start(pi2,&r);
  LAP_Se_SetPartitionMode(NORMAL,&r);
  while(1) {
    g2 = g2 - 1;  // RACE!
  }
  return 0;
}
