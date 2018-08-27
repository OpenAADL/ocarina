/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2017 ESA & ISAE.
 */

#ifndef __PO_HI_TYPES_H_
#define __PO_HI_TYPES_H_

#include"po_hi_config.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#define __PO_HI_UNUSED_NODE -1

/*
 * Types are configured according to the ones available
 * on the target host.
 */

#ifdef HAVE_STDBOOL_H
typedef bool __po_hi_bool_t;
#else
#error This configuration is not supported, PolyORB-HI/C requires a C compiler supporting the "bool" type.
#endif

typedef float  __po_hi_float32_t;
typedef double __po_hi_float64_t;

#ifdef HAVE_STDINT_H
  typedef int8_t     __po_hi_int8_t;
  typedef int16_t    __po_hi_int16_t;
  typedef int32_t    __po_hi_int32_t;
#ifndef COMPCERT
  typedef int64_t    __po_hi_int64_t;
#endif
  typedef uint8_t    __po_hi_uint8_t;
  typedef uint16_t   __po_hi_uint16_t;
  typedef uint32_t   __po_hi_uint32_t;
#ifndef COMPCERT
  typedef uint64_t   __po_hi_uint64_t;
#endif
#else

/*
 * Most modern compilers have stdint.h header file.
 */

#error This configuration is not supported, PolyORB-HI/C requires a C compiler with support for the stdint.h header file.

  #if SIZEOF_INT == 4
  typedef int                    __po_hi_int32_t;
  #elif SIZEOF_LONG_INT == 4
  typedef long int               __po_hi_int32_t;
  #elif SIZEOF_SHORT_INT == 4
  typedef short int              __po_hi_int32_t;
  #endif

  #if SIZEOF_INT == 2
  typedef int                    __po_hi_int16_t;
  typedef unsigned int           __po_hi_uint16_t;
  #elif SIZEOF_SHORT_INT == 2
  typedef short int              __po_hi_int16_t;
  typedef unsigned short int     __po_hi_uint16_t;
  #elif SIZEOF_LONG_INT == 2
  typedef long int               __po_hi_int16_t;
  typedef unsigned long int      __po_hi_uint16_t;
  #endif

  #if SIZEOF_CHAR == 1
    typedef char                 __po_hi_int8_t;
    typedef unsigned char        __po_hi_uint8_t;
  #endif
#endif

typedef unsigned char            __po_hi_byte_t;

typedef __po_hi_int32_t          __po_hi_port_id_t;

typedef enum
{
   __PO_HI_IN_DATA_INTER_PROCESS             = 0,
   __PO_HI_OUT_DATA_INTER_PROCESS            = 2,
   __PO_HI_IN_DATA_INTRA_PROCESS             = 4,
   __PO_HI_OUT_DATA_INTRA_PROCESS            = 6,
   __PO_HI_IN_EVENT_DATA_INTER_PROCESS       = 8,
   __PO_HI_OUT_EVENT_DATA_INTER_PROCESS      = 10,
   __PO_HI_IN_EVENT_DATA_INTRA_PROCESS       = 12,
   __PO_HI_OUT_EVENT_DATA_INTRA_PROCESS      = 14,
   __PO_HI_IN_EVENT_INTER_PROCESS            = 16,
   __PO_HI_OUT_EVENT_INTER_PROCESS           = 18,
   __PO_HI_IN_EVENT_INTRA_PROCESS            = 20,
   __PO_HI_OUT_EVENT_INTRA_PROCESS           = 22,
   __PO_HI_INVALID_PORT_KIND                 = 50
}__po_hi_port_kind_t;

void __po_hi_copy_array (void* dst, void* src, __po_hi_uint32_t size);

#endif /* __PO_HI_TYPES_H_ */
