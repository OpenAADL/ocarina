/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.
 */

#ifndef __PO_HI_PROTECTED_H__
#define __PO_HI_PROTECTED_H__


#include <stdint.h>
#include <deployment.h>

#define __PO_HI_PROTECTED_TYPE_REGULAR    0
#define __PO_HI_PROTECTED_TYPE_PIP        1
#define __PO_HI_PROTECTED_TYPE_PCP        2

#if defined (POSIX) || defined (RTEMS_POSIX) || defined (XENO_POSIX)
   #include <stdlib.h>
   #include <stdint.h>
   #include <time.h>
   #include <pthread.h>
#endif

#if defined (__PO_HI_RTEMS_CLASSIC_API)
   #include <rtems.h>
#endif

#if defined (XENO_NATIVE)
   #include <native/mutex.h>
#endif

#ifdef _WIN32
#include <windows.h>
#endif

typedef enum
{
   __PO_HI_PROTECTED_REGULAR     = 1,
   __PO_HI_MUTEX_REGULAR         = 1,
   __PO_HI_PROTECTED_PIP         = 2,
   __PO_HI_MUTEX_PIP             = 2,
   __PO_HI_PROTECTED_PCP         = 3,
   __PO_HI_MUTEX_PCP             = 3,
   __PO_HI_PROTECTED_IPCP        = 4,
   __PO_HI_MUTEX_IPCP            = 4,
   __PO_HI_PROTECTED_INVALID     = 1
}__po_hi_protected_protocol_t;

typedef __po_hi_protected_protocol_t __po_hi_mutex_protocol_t;

typedef struct
{
   __po_hi_mutex_protocol_t   protocol;
   int                        priority;
#if defined (POSIX) || defined (RTEMS_POSIX) || defined (XENO_POSIX)
   pthread_mutex_t      posix_mutex;
   pthread_mutexattr_t  posix_mutexattr;
#endif
#if defined (__PO_HI_RTEMS_CLASSIC_API)
   rtems_id             rtems_mutex;
#endif
#if defined (XENO_NATIVE)
   RT_MUTEX             xeno_mutex;
#endif
#if defined (_WIN32)
   HANDLE               win32_mutex;
#endif
}__po_hi_mutex_t;

typedef uint8_t __po_hi_protected_t;

/*
 * \fn __po_hi_protected_lock
 *
 * \brief Lock the variable which has he id given by the argument.
 *
 * Return __PO_HI_SUCCESS if it is successfull.  If there is an error,
 * it can return __PO_HI_ERROR_PTHREAD_MUTEX value
 */
int __po_hi_protected_lock (__po_hi_protected_t protected_id);

/**
 * \fn __po_hi_protected_lock
 *
 * Unlock the variable which has he id given
 * by the argument.
 * Return __PO_HI_SUCCESS if it is successfull.
 * If there is an error, it can return
 * __PO_HI_ERROR_PTHREAD_MUTEX value
 */
int __po_hi_protected_unlock (__po_hi_protected_t protected_id);

/**
 * \fn __po_hi_protected_init
 *
 * \brief Initialize all variables to handle protected objects in PolyORB-HI-C
 */
int __po_hi_protected_init (void);


/**
 * \fn __po_hi_mutex_init
 *
 * \brief Initialize a mutex no matter the underlying executive
 *
 * This function allocate all the resources to the mutex so that it can be
 * used with __po_hi_mutex_lock() and __po_hi_mutex_unlock(). The second
 * parameter is the locking protocol of the mutex that is mapped on the
 * appropriate underlyign OS directives if supported. The third argument
 * is the priority ceiling used, only relevant if the protocol
 * needs such an option. Otherwise, any value can be used.
 *
 * Upon success, the function returns __PO_HI_SUCCESS.
 * It returns the following potential values:
 *  - __PO_HI_SUCCESS: successful operation
 *  - __PO_HI_TOOMANY: too many resources allocated at this time
 *  - __PO_HI_INVALID: supplied argument value (memory error)
 */
int __po_hi_mutex_init (__po_hi_mutex_t* mutex, const __po_hi_mutex_protocol_t protocol, const int priority);

/**
 * \fn __po_hi_mutex_lock
 *
 * \brief Lock a mutex no matter the underlying executive
 *
 * This function locks the mutex so that it ensures that only one task
 * acquired it. Note that if the mutex was previously acquired, the caller
 * will be blocked until the mutex is released.
 *
 * Upon success, the function returns __PO_HI_SUCCESS.
 * It returns the following potential values:
 *  - __PO_HI_SUCCESS: successful operation
 *  - __PO_HI_INVALID: supplied argument value (memory error)
 *  - __PO_HI_NOTINITALIZED: supplied resources was not initialized
 */
int __po_hi_mutex_lock (__po_hi_mutex_t* mutex);

/**
 * \fn __po_hi_mutex_unlock
 *
 * \brief Unlock a mutex no matter the underlying executive
 *
 * This function unlocks the mutex so that other tasks can acquire it
 * again.
 *
 * Upon success, the function returns __PO_HI_SUCCESS.
 * It returns the following potential values:
 *  - __PO_HI_SUCCESS: successful operation
 *  - __PO_HI_INVALID: supplied argument value (memory error)
 *  - __PO_HI_NOTINITALIZED: supplied resources was not initialized
 */

int __po_hi_mutex_unlock (__po_hi_mutex_t* mutex);


#endif /*  __PO_HI_PROTECTED_H__ */
