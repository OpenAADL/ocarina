/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2010-2017 ESA & ISAE.
 */

#ifndef __PO_HI_TASK_H__
#define __PO_HI_TASK_H__

/*
 * Define some values that are dependant of the
 * underlying executive.
 */
#if defined (POSIX) || defined (XENO_POSIX) || defined (SIMULATOR)
   #include <stdlib.h>
   #include <stdio.h>
#if defined (DLL)
   #define __PO_HI_MAIN_NAME aadl_start
#else
   #define __PO_HI_MAIN_NAME main
#endif
   #define __PO_HI_MAIN_TYPE int
   #define __PO_HI_MAIN_ARGS int argc , char *argv[] , char **arge
   #define __PO_HI_MAIN_RETURN EXIT_SUCCESS
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)

#elif defined(_WIN32)
   #include <tchar.h>
   #include <windows.h>
   #include <stdlib.h>
   #include <stdio.h>
   #define __PO_HI_MAIN_NAME _tmain
   #define __PO_HI_MAIN_TYPE int
   #define __PO_HI_MAIN_ARGS
   #define __PO_HI_MAIN_RETURN EXIT_SUCCESS
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)

#elif defined (XENO_NATIVE)
   #include <native/task.h>
   #include <native/timer.h>
   #define __PO_HI_MAIN_NAME main
   #define __PO_HI_MAIN_TYPE int
   #define __PO_HI_MAIN_ARGS int argc , char *argv[] , char **arge
   #define __PO_HI_MAIN_RETURN 0
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)

#elif defined(__PO_HI_RTEMS_CLASSIC_API)
   #define __PO_HI_MAIN_NAME Init
   #define __PO_HI_MAIN_TYPE rtems_task
   #define __PO_HI_MAIN_ARGS rtems_task_argument argument
   #define __PO_HI_MAIN_RETURN 0
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)

#elif defined(RTEMS_POSIX)
   #define __PO_HI_MAIN_NAME POSIX_Init
   #define __PO_HI_MAIN_TYPE int
   #define __PO_HI_MAIN_ARGS
   #define __PO_HI_MAIN_RETURN 0
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)

#elif defined (FREERTOS)
   #define __PO_HI_MAIN_NAME main
   #define __PO_HI_MAIN_TYPE int
   #define __PO_HI_MAIN_ARGS int argc , char *argv[] , char **arge
   #define __PO_HI_MAIN_RETURN 0
   #define __ERRORMSG(s, args...) fprintf(stderr, s, ##args)
#endif

#if defined(POSIX) || defined (RTEMS_POSIX) || defined (XENO_POSIX)
   #include <semaphore.h>
   #include <po_hi_time.h>
   #include <pthread.h>
   #include <sched.h>
   #define __PO_HI_MAX_PRIORITY sched_get_priority_max(SCHED_FIFO)
   #define __PO_HI_MIN_PRIORITY sched_get_priority_min(SCHED_FIFO)
   #define __PO_HI_DEFAULT_PRIORITY ((sched_get_priority_min(SCHED_FIFO) + sched_get_priority_max(SCHED_FIFO))/2)

#elif defined (SIMULATOR)
   #include <po_hi_time.h>
   #define __PO_HI_MAX_PRIORITY 255
   #define __PO_HI_MIN_PRIORITY 1
   #define __PO_HI_DEFAULT_PRIORITY 128

#elif defined(_WIN32)
   #include <inttypes.h>
   #include <po_hi_time.h>
   #define __PO_HI_DEFAULT_PRIORITY 0
   #define __PO_HI_MAX_PRIORITY 2
   #define __PO_HI_MIN_PRIORITY 0

#elif defined(__PO_HI_RTEMS_CLASSIC_API)
   #include <rtems.h>
   #include <inttypes.h>
   #include <po_hi_time.h>
   #define __PO_HI_DEFAULT_PRIORITY RTEMS_NO_PRIORITY
   #define __PO_HI_MAX_PRIORITY RTEMS_NO_PRIORITY
   #define __PO_HI_MIN_PRIORITY RTEMS_NO_PRIORITY
#elif defined(XENO_NATIVE)
   #include <inttypes.h>
   #include <po_hi_time.h>
   #define __PO_HI_DEFAULT_PRIORITY 50
   #define __PO_HI_MAX_PRIORITY 99
   #define __PO_HI_MIN_PRIORITY 0
#elif defined(FREERTOS)
   #include <po_hi_time.h>
   #define __PO_HI_DEFAULT_PRIORITY 50
   #define __PO_HI_MAX_PRIORITY 99
   #define __PO_HI_MIN_PRIORITY 0
#endif

#include <stddef.h>

#include <po_hi_types.h>
#include <deployment.h>

typedef __po_hi_uint16_t __po_hi_priority_t;
typedef size_t __po_hi_stack_t;

/*
 * Initialize tasking entities
 * Returns SUCCESS if there is no error.
 */
int __po_hi_initialize_tasking(void);

/*
 * Get the id of a task
 */
__po_hi_task_id __po_hi_get_task_id (void);

/*
 * Create a periodic task.
 *
 * The task created have the identifier given by the first
 * parameter. It is created according to the period created
 * with __po_hi_* functions (like __po_hi_milliseconds())
 * and priority parameters (use the OS priority). The task execute
 * periodically start_routine.
 *
 * This function returns SUCCESS if there is no error. Else,
 * it returns the negative value ERROR_CREATE_TASK.
 */
int __po_hi_create_periodic_task (const __po_hi_task_id      id,
                                  const __po_hi_time_t*      period,
                                  const __po_hi_priority_t   priority,
                                  const __po_hi_stack_t      stack_size,
                                  const __po_hi_int8_t      core_id,
                                  void*                (*start_routine)(void));

/*
 * Create a sporadic task.
 *
 * The identifier of the task is the first parameter. The period and
 * the priority of the task are stored in the second and third
 * parameter.  The code executed by the task is stored in the
 * start_routine pointer.
 *
 * Returns SUCCESS value if there is no error. Else, returns the negative
 * value ERROR_CREATE_TASK
 */
int __po_hi_create_sporadic_task (const __po_hi_task_id      id,
				                      const __po_hi_time_t*      period,
                                  const __po_hi_priority_t   priority,
                                  const __po_hi_stack_t      stack_size,
                                  const __po_hi_int8_t       core_id,
                                  void*                      (*start_routine)(void));

/*
 * Create a generic task
 *
 * The identifier of the task is the first parameter. The period and
 * the priority of the task are stored in the second and third
 * parameter.  The code executed by the task is stored in the
 * start_routine pointer.
 *
 * Returns SUCCESS value if there is no error. Else, returns the negative
 * value ERROR_CREATE_TASK
 */
int __po_hi_create_generic_task (const __po_hi_task_id      id,
                                 const __po_hi_time_t*      period,
                                 const __po_hi_priority_t   priority,
                                 const __po_hi_stack_t      stack_size,
                                 const __po_hi_int8_t      core_id,
                                 void*                      (*start_routine)(void),
                                 void*                      arg);

/*
 * Wait the end of all tasks.
 * This function typically never ends, because all tasks
 * are doing an infinite loop and never ends. It just
 * used to avoid an infinite loop in the main thread.
 */
void __po_hi_wait_for_tasks (void);

/*
 * Called by a periodic task, to wait for its next period
 * The argument is the task identifier
 * Returns SUCCESS value, and if fails, returns a negative value
 */
int __po_hi_wait_for_next_period (__po_hi_task_id task);

/*
 * Sleep until the time given in argument. The second
 * argument is the task identifier which will sleep.
 * Return SUCCESS if there is no error. Else, it returns
 * a negative value : ERROR_CLOCK or ERROR_PTHREAD_COND
 */
 int __po_hi_task_delay_until (__po_hi_time_t* time, __po_hi_task_id task);

/*
 * Computer the next period for a task, according to the period
 * argument given at initialization time. The argument task
 * is the task-identifier in the node (__po_hi_task_id type).
 */
 int __po_hi_compute_next_period (__po_hi_task_id task);

 /*
  * Delete all the tasks that were created within the system.
  */
void __po_hi_tasks_killall (void);

 /*
  * Wait a given amount of time.
  */
void __po_hi_task_wait_offset (const __po_hi_time_t* time);


#endif /* __PO_HI_TASK_H__ */
