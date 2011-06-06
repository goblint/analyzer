#ifndef APP_HEADER_H
#define APP_HEADER_H

#include "tpl_app_objects.h"

/*=============================================================================
 * Declaration of event related defines and structures
 */
/*
 * event pong_deployed
 */
#define mask_of_pong_deployed 1
#define pong_deployed mask_of_pong_deployed

/*
 * event ping_deployed
 */
#define mask_of_ping_deployed 2
#define ping_deployed mask_of_ping_deployed



/*=============================================================================
 * Declaration of Resource related defines and structures
 */

#define resource_id_of_cubbyHoleMutex  0
#define cubbyHoleMutex  resource_id_of_cubbyHoleMutex


/*=============================================================================
 * Declaration of Task related defines and structures
 */

#define task_id_of_ping  0
#define ping  task_id_of_ping
#define task_id_of_pong  1
#define pong  task_id_of_pong
#define task_id_of_shutdown  2
#define shutdown  task_id_of_shutdown


/*=============================================================================
 * Declaration of ISR2 related defines and structures
 */



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
#define alarm_id_of_StartPingPong  1
#define StartPingPong  alarm_id_of_StartPingPong


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
