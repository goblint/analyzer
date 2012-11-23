#ifndef APP_HEADER_H
#define APP_HEADER_H

#include "tpl_app_objects.h"

/*=============================================================================
 * Declaration of event related defines and structures
 */


/*=============================================================================
 * Declaration of Resource related defines and structures
 */

#define resource_id_of_Counter  0
#define Counter  resource_id_of_Counter
#define resource_id_of_Counter2  1
#define Counter2  resource_id_of_Counter2
#define resource_id_of_Mutex  2
#define Mutex  resource_id_of_Mutex


/*=============================================================================
 * Declaration of Task related defines and structures
 */

#define task_id_of_printer  0
#define printer  task_id_of_printer
#define task_id_of_shutdown  1
#define shutdown  task_id_of_shutdown

#define TASK( TaskName )    void  function_of_##TaskName( void )
#define ISR( TaskName )    void  function_of_##TaskName( void )

/*=============================================================================
 * Declaration of ISR2 related defines and structures
 */

#define isr_id_of_PosixSignal_USR2  0
#define PosixSignal_USR2  isr_id_of_PosixSignal_USR2


/*=============================================================================
 * Declaration of Counters related defines and structures
 */
#define OSTICKSPERBASE_counter100ms 2
#define OSMAXALLOWEDVALUE_counter100ms 255
#define OSMINCYCLE_counter100ms 1



/*=============================================================================
 * Declaration of Alarm related defines and structures
 */

#define alarm_id_of_ShutdownTask  0
#define ShutdownTask  alarm_id_of_ShutdownTask


/*=============================================================================
 * Declaration of flags macros
 */
 

/*=============================================================================
 * Declaration of messages identifiers
 */






#ifdef WITH_AUTOSAR
/*=============================================================================
 * Declaration of schedule tables related defines and structures
 */
$SCHEDULETABLES$

/*=============================================================================
 * Declaration of counters ids
 */


#endif

#endif

/* End of file tpl_os_generated_configuration.h */
