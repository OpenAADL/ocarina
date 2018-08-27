/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.
 */

#ifndef __PO_HI_RETURNS_H__
#define __PO_HI_RETURNS_H__

/* Success return code */
#define __PO_HI_SUCCESS                    1
#define __PO_HI_UNAVAILABLE                2
#define __PO_HI_INVALID                    5
#define __PO_HI_TOOMANY                    6

#define __PO_HI_NOTIMPLEMENTED             8

#define __PO_HI_NOTINITIALIZED             9

/* Errors from the API */
#define __PO_HI_ERROR_CREATE_TASK         -10
#define __PO_HI_ERROR_TASK_PERIOD         -11
#define __PO_HI_ERROR_CLOCK               -15
#define __PO_HI_ERROR_QUEUE_FULL          -20

#define __PO_HI_ERROR_UNKNOWN             -30

/* Errors related to the pthread library */
#define __PO_HI_ERROR_PTHREAD_COND        -50
#define __PO_HI_ERROR_PTHREAD_MUTEX       -51
#define __PO_HI_ERROR_PTHREAD_CREATE      -52
#define __PO_HI_ERROR_PTHREAD_ATTR        -53
#define __PO_HI_ERROR_PTHREAD_SCHED       -54
#define __PO_HI_ERROR_TRANSPORT_SEND      -55
#define __PO_HI_ERROR_PTHREAD_BARRIER     -56

/* Errors related to the protected and semaphore API */
#define __PO_HI_ERROR_PROTECTED_LOCK      -60
#define __PO_HI_ERROR_PROTECTED_UNLOCK    -61
#define __PO_HI_ERROR_PROTECTED_CREATE    -62

#define __PO_HI_ERROR_MUTEX_LOCK          -60
#define __PO_HI_ERROR_MUTEX_UNLOCK        -61
#define __PO_HI_ERROR_MUTEX_CREATE        -62

#define __PO_HI_ERROR_SEM_WAIT            -60
#define __PO_HI_ERROR_SEM_RELEASE         -61
#define __PO_HI_ERROR_SEM_CREATE          -62

/* GIOP error code */
#define __PO_HI_GIOP_INVALID_SIZE         -100
#define __PO_HI_GIOP_INVALID_VERSION      -120
#define __PO_HI_GIOP_INVALID_REQUEST_TYPE -150
#define __PO_HI_GIOP_INVALID_OPERATION    -180
#define __PO_HI_GIOP_UNSUPPORTED          -200

#define __PO_HI_ERROR_EXISTS              -80
#define __PO_HI_ERROR_NOEXISTS            -81

#endif /* __RETURNS_H__ */
