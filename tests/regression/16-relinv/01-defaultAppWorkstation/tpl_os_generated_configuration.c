#include "tpl_os_internal_types.h"
#include "tpl_os_definitions.h"
#include "tpl_machine.h"
#include "tpl_os_it.h"
#include "tpl_os_it_kernel.h"
#include "tpl_os_alarm_kernel.h"
#include "tpl_os_action.h"

#ifndef WITH_AUTOSAR
#include "tpl_com_notification.h"
#include "tpl_com_mo.h"
#include "tpl_com_internal_com.h"
#include "tpl_com_app_copy.h"
#include "tpl_com_filters.h"
#endif

#include "tpl_os_generated_configuration.h"
#include "tpl_app_objects.h"

#ifdef WITH_AUTOSAR
#include "tpl_as_st_kernel.h"
#include "tpl_as_action.h"
#endif

/*=============================================================================
 * Definition and initialization of event related defines and structures
 */


/*=============================================================================
 * Definition and initialization of Resource related structures
 */

/*
 * Resource descriptor of resource Mutex
 *
 * Tasks which use this resource :
 * $TASKS$
 *
 * ISRs which use this resource :
 * $ISRS$
 */
tpl_resource descriptor_of_resource_Mutex = {
    /* ceiling priority of the resource */  (tpl_priority)3,
    /* owner previous priority          */  (tpl_priority)0,
    /* owner of the resource            */  NULL,
    /* next resource in the list        */  NULL
};


/*
 * Resource descriptor of resource Counter
 *
 * Tasks which use this resource :
 * $TASKS$
 *
 * ISRs which use this resource :
 * $ISRS$
 */
tpl_resource descriptor_of_resource_Counter = {
    /* ceiling priority of the resource */  (tpl_priority)1,
    /* owner previous priority          */  (tpl_priority)0,
    /* owner of the resource            */  NULL,
    /* next resource in the list        */  NULL
};


/*
 * Resource descriptor of resource Counter2
 *
 * Tasks which use this resource :
 * $TASKS$
 *
 * ISRs which use this resource :
 * $ISRS$
 */
tpl_resource descriptor_of_resource_Counter2 = {
    /* ceiling priority of the resource */  (tpl_priority)1,
    /* owner previous priority          */  (tpl_priority)0,
    /* owner of the resource            */  NULL,
    /* next resource in the list        */  NULL
};


tpl_resource *tpl_resource_table[RESOURCE_COUNT] = {
    (tpl_resource *)&descriptor_of_resource_Counter,
    (tpl_resource *)&descriptor_of_resource_Counter2,
    (tpl_resource *)&descriptor_of_resource_Mutex
};


/*=============================================================================
 * Definition and initialization of Task related defines and structures
 */

/*
 * printer stack
 */
tpl_stack_word stack_zone_of_printer[32768/sizeof(tpl_stack_word)];

#define STACK_OF_TASK_printer { stack_zone_of_printer, 32768 }

/*
 * printer context
 */
tpl_context integer_context_of_printer;

#define CONTEXT_OF_TASK_printer &integer_context_of_printer
/*
 * Task printer function prototype
 */
void function_of_task_printer(void);

#ifdef WITH_AUTOSAR_TIMING_PROTECTION

#endif

/*
 * Static descriptor of task printer
 */
tpl_exec_static static_descriptor_of_task_printer = {
    /* context                  */  CONTEXT_OF_TASK_printer,
    /* stack                    */  STACK_OF_TASK_printer,
    /* entry point (function)   */  function_of_task_printer,
    /* internal ressource       */  NULL,
    /* task id                  */  task_id_of_printer,
    /* task base priority       */  (tpl_priority)1,
    /* max activation count     */  1,
    /* task type                */  TASK_BASIC,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* pointer to the timing
       protection descriptor    */  NULL
#endif
};

/*
 * Dynamic descriptor of task printer
 */
tpl_task descriptor_of_task_printer = {
    {       /* beginning of exec_desc part */
    /* static descriptor    */  &static_descriptor_of_task_printer,
    /* resources            */  NULL,
    /* activate count       */  0,
    /* task priority        */  (tpl_priority)1,
    /* task state           */  AUTOSTART,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* start date           */  0,
    /* time left            */  0,
#endif
    },    /* end of exec_desc part */
    /* event mask           */  0,
    /* event wait           */  0
};


/*
 * shutdown stack
 */
tpl_stack_word stack_zone_of_shutdown[32768/sizeof(tpl_stack_word)];

#define STACK_OF_TASK_shutdown { stack_zone_of_shutdown, 32768 }

/*
 * shutdown context
 */
tpl_context integer_context_of_shutdown;

#define CONTEXT_OF_TASK_shutdown &integer_context_of_shutdown
/*
 * Task shutdown function prototype
 */
void function_of_task_shutdown(void);

#ifdef WITH_AUTOSAR_TIMING_PROTECTION

#endif

/*
 * Static descriptor of task shutdown
 */
tpl_exec_static static_descriptor_of_task_shutdown = {
    /* context                  */  CONTEXT_OF_TASK_shutdown,
    /* stack                    */  STACK_OF_TASK_shutdown,
    /* entry point (function)   */  function_of_task_shutdown,
    /* internal ressource       */  NULL,
    /* task id                  */  task_id_of_shutdown,
    /* task base priority       */  (tpl_priority)2,
    /* max activation count     */  1,
    /* task type                */  TASK_BASIC,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* pointer to the timing
       protection descriptor    */  NULL
#endif
};

/*
 * Dynamic descriptor of task shutdown
 */
tpl_task descriptor_of_task_shutdown = {
    {       /* beginning of exec_desc part */
    /* static descriptor    */  &static_descriptor_of_task_shutdown,
    /* resources            */  NULL,
    /* activate count       */  0,
    /* task priority        */  (tpl_priority)2,
    /* task state           */  SUSPENDED,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* start date           */  0,
    /* time left            */  0,
#endif
    },    /* end of exec_desc part */
    /* event mask           */  0,
    /* event wait           */  0
};


tpl_task *tpl_task_table[TASK_COUNT] = {
    (tpl_task *)&descriptor_of_task_printer,
    (tpl_task *)&descriptor_of_task_shutdown
};


/*=============================================================================
 * Definition and initialization of ISR2 related defines and structures
 */

/*
 * PosixSignal_USR2 stack
 */
tpl_stack_word stack_zone_of_PosixSignal_USR2[32768/sizeof(tpl_stack_word)];

#define STACK_OF_ISR_PosixSignal_USR2 { stack_zone_of_PosixSignal_USR2, 32768 }

/*
 * PosixSignal_USR2 context
 */
tpl_context integer_context_of_PosixSignal_USR2;

#define CONTEXT_OF_ISR_PosixSignal_USR2 &integer_context_of_PosixSignal_USR2
/*
 * ISR PosixSignal_USR2 function prototype
 */
void function_of_isr_PosixSignal_USR2(void);

#ifdef WITH_AUTOSAR_TIMING_PROTECTION

#endif

/*
 * Static descriptor of ISR PosixSignal_USR2
 */
tpl_exec_static static_descriptor_of_isr_PosixSignal_USR2 = {
    /* context                  */ CONTEXT_OF_ISR_PosixSignal_USR2,
    /* stack                    */ STACK_OF_ISR_PosixSignal_USR2,
    /* entry point (function)   */ function_of_isr_PosixSignal_USR2,
    /* internal ressource       */ NULL,
    /* isr id                   */ 0,
    /* isr base priority        */ (tpl_priority)3,
    /* max activation count     */ 1,
    /* isr type                 */ IS_ROUTINE,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* pointer to the timing
       protection descriptor    */  NULL
#endif
};

tpl_isr_static static_helper_of_isr_PosixSignal_USR2 = {
	/* helper */ NULL,
	/* next */   NULL
};

/*
 * Dynamic descriptor of ISR PosixSignal_USR2
 */
tpl_isr descriptor_of_isr_PosixSignal_USR2 = {
    {       /* beginning of exec_desc part */
    /* static descriptor    */  &static_descriptor_of_isr_PosixSignal_USR2,
    /* resources            */  NULL,
    /* activate count       */  0,
    /* isr priority         */  (tpl_priority)3,
    /* isr state            */  SUSPENDED,
#ifdef WITH_AUTOSAR_TIMING_PROTECTION
    /* start date           */  0,
    /* time left            */  0,
#endif
    },    /* end of exec_desc part */
    /* more static desc     */  &static_helper_of_isr_PosixSignal_USR2
#ifdef WITH_AUTOSAR
    ,
    /* enabled field        */  TRUE
#endif
};


tpl_isr *tpl_isr_table[ISR_COUNT] = {
    (tpl_isr *)&descriptor_of_isr_PosixSignal_USR2
};
#if ISR_COUNT > 0
#include <signal.h>

int signal_for_isr_id[] = {
	SIGUSR2
};
#endif


/*=============================================================================
 * Definition and initialization of Counters related defines and structures
 */

/*
 * Counter descriptor of counter counter100ms
 */
tpl_counter descriptor_of_counter_counter100ms = {
    /* ticks per base       */  2,
    /* max allowed value    */  255,
    /* minimum cycle        */  1,
    /* current tick         */  0,
    /* current date         */  0,
    /* first alarm          */  NULL,
    /* next alarm to raise  */  NULL
};


void tpl_schedule(int from);
tpl_status tpl_counter_tick(tpl_counter *counter);

void tpl_call_counter_tick()
{
	tpl_status  need_rescheduling = NO_SPECIAL_CODE;
    need_rescheduling |= tpl_counter_tick(&descriptor_of_counter_counter100ms);

	if (need_rescheduling == NEED_RESCHEDULING) {
		tpl_schedule(FROM_IT_LEVEL);
    }
}



/*=============================================================================
 * Definition and initialization of Alarm related structures
 */

/*
 * Alarm descriptor of alarm ShutdownTask
 */
tpl_task_activation_action task_act_of_ShutdownTask = {
    {
        /* action function  */  tpl_action_activate_task
    },
    /* task descriptor ptr  */  &descriptor_of_task_shutdown
};

tpl_alarm_static stat_descriptor_of_alarm_ShutdownTask = {
    {
        /* pointer to counter           */  &descriptor_of_counter_counter100ms,
        /* pointer to the expiration    */  tpl_raise_alarm
    },
    /* action of the alarm  */  (tpl_action *)&task_act_of_ShutdownTask
};

tpl_time_obj descriptor_of_alarm_ShutdownTask = {
    /* pointer to the static part   */  (tpl_time_obj_static *)&stat_descriptor_of_alarm_ShutdownTask,
    /* next alarm                   */  NULL,
    /* prev alarm                   */  NULL,
    /* cycle                        */  0,
    /* date                         */  100,
    /* State of the alarm           */  ALARM_AUTOSTART
};

tpl_time_obj *tpl_alarm_table[ALARM_COUNT] = {
    (tpl_time_obj *)&descriptor_of_alarm_ShutdownTask
};


/*=============================================================================
 * Declaration of flags functions
 */


/*=============================================================================
 * Definition and initialization of Messages related structures
 */




#ifdef WITH_AUTOSAR
/*=============================================================================
 * Declaration of schedule tables related defines and structures
 */
$SCHEDULETABLES$
#endif

/*=============================================================================
 * Definition and initialization of Ready List structures
 */
tpl_exec_common *tpl_priority_0_fifo[1];
tpl_exec_common *tpl_priority_1_fifo[1];
tpl_exec_common *tpl_priority_2_fifo[2];
tpl_exec_common *tpl_priority_3_fifo[2];

tpl_fifo_state tpl_fifo_rw[4] = {
    { 0 , 0 },
    { 0 , 0 },
    { 0 , 0 },
    { 0 , 0 }
};

tpl_priority_level tpl_ready_list[4] = {
    { tpl_priority_0_fifo , 1 },
    { tpl_priority_1_fifo , 1 },
    { tpl_priority_2_fifo , 2 },
    { tpl_priority_3_fifo , 2 }
};



/* End of file tpl_os_generated_configuration.c */
