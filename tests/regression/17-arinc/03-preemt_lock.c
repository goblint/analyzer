// SKIP PARAMS: --sets ana.activated[0][+] arinc 

typedef char * SEMAPHORE_NAME_TYPE;
typedef void * SEMAPHORE_ID_TYPE;
typedef int    RETURN_CODE_TYPE;
typedef int    SEMAPHORE_VALUE_TYPE;
typedef void * QUEUING_DISCIPLINE_TYPE;
typedef int    SYSTEM_TIME_TYPE;

extern void LAP_Se_GetSemaphoreId(SEMAPHORE_NAME_TYPE, SEMAPHORE_ID_TYPE*, RETURN_CODE_TYPE*);
extern void LAP_Se_CreateSemaphore(SEMAPHORE_NAME_TYPE,SEMAPHORE_VALUE_TYPE,SEMAPHORE_VALUE_TYPE,QUEUING_DISCIPLINE_TYPE,SEMAPHORE_ID_TYPE*,RETURN_CODE_TYPE*);
extern void LAP_Se_WaitSemaphore(SEMAPHORE_ID_TYPE,SYSTEM_TIME_TYPE,RETURN_CODE_TYPE*);
extern void LAP_Se_SignalSemaphore(SEMAPHORE_ID_TYPE,RETURN_CODE_TYPE*);

typedef void * PROCESS_NAME_TYPE;
typedef void * SYSTEM_ADDRESS_TYPE;
typedef long   STACK_SIZE_TYPE;
typedef long   PRIORITY_TYPE;
typedef long   SYSTEM_TIME_TYPE;
typedef long   SYSTEM_TIME_TYPE;
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

extern void LAP_Se_CreateProcess(PROCESS_ATTRIBUTE_TYPE, PROCESS_ID_TYPE*, RETURN_CODE_TYPE*);

extern void LAP_Se_Start(PROCESS_ID_TYPE, RETURN_CODE_TYPE*);

typedef int LOCK_LEVEL_TYPE;
  
extern void LAP_Se_LockPreemption (
       /*out*/ LOCK_LEVEL_TYPE  *LOCK_LEVEL,
       /*out*/ RETURN_CODE_TYPE *RETURN_CODE );
  
extern void LAP_Se_UnlockPreemption (
       /*out*/ LOCK_LEVEL_TYPE   *LOCK_LEVEL,
       /*out*/ RETURN_CODE_TYPE  *RETURN_CODE );
  
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
  
// -----------------------

extern void assert(int);
int g;

void P1(void){
 LOCK_LEVEL_TYPE ll;
 RETURN_CODE_TYPE r;
 while (1){
   LAP_Se_LockPreemption(&ll,&r);
   g = 1;
   assert(g==1);
   LAP_Se_UnlockPreemption(&ll,&r);
 }
 return;
}

int main(){
 RETURN_CODE_TYPE r;
 PROCESS_ID_TYPE pi1,pi2;
 PROCESS_ATTRIBUTE_TYPE p1,p2;
 p1.ENTRY_POINT = (void *) &P1;
 p1.BASE_PRIORITY = 10;
 LAP_Se_CreateProcess(p1,&pi1,&r);
 LAP_Se_Start(pi1,&r);
 LAP_Se_SetPartitionMode(NORMAL,&r);
 return 0;
}