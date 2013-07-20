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

int g,h;
SEMAPHORE_ID_TYPE sem_id;

void P1(void){
 RETURN_CODE_TYPE r;
 while (1){
   g = g + 1; // NOWARN!
 }
 return;
}

void P2(void){
 RETURN_CODE_TYPE r;
 while (1){
   h = h + 1; // NOWARN!
 }
 return;
}


int main(){
 RETURN_CODE_TYPE r;
 PROCESS_ID_TYPE pi1, pi2;
 SEMAPHORE_ID_TYPE sem_id_local;
 PROCESS_ATTRIBUTE_TYPE p1, p2;
 LAP_Se_CreateSemaphore("my_mutex",1,1,0,&sem_id_local,&r);
 LAP_Se_GetSemaphoreId("my_mutex",&sem_id,&r);
 p1.ENTRY_POINT = (void *) &P1;
 p1.BASE_PRIORITY = 10;
 p2.ENTRY_POINT = (void *) &P2;
 p2.BASE_PRIORITY = 20;
 LAP_Se_CreateProcess(p1,&pi1,&r);
 LAP_Se_CreateProcess(p2,&pi2,&r);
 LAP_Se_Start(pi1,&r);
 LAP_Se_Start(pi2,&r);
 LAP_Se_SetPartitionMode(NORMAL,&r);
 return 0;
}