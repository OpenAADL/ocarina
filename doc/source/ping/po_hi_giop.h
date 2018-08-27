/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.
 */

#ifdef __PO_HI_USE_GIOP

#ifndef __PO_HI_GIOP_H__
#define __PO_HI_GIOP_H__

#include <po_hi_types.h>
#include <po_hi_messages.h>
#include <deployment.h>
#include <string.h>

/*
 * This file defines the structures and functions to support the GIOP
 * protocol. The supported verrsion of GIOP is the 1.3.
 *
 * This implementation was made according to the CORBA 3.1, Part 2,
 * chapter 9 specifications.
 */

#define __PO_HI_GIOP_MSGTYPE_REQUEST            0
#define __PO_HI_GIOP_MSGTYPE_REPLY              1
#define __PO_HI_GIOP_MSGTYPE_CANCELREQUEST      2
#define __PO_HI_GIOP_MSGTYPE_LOCATEREQUEST      3
#define __PO_HI_GIOP_MSGTYPE_LOCATEREPLY        4
#define __PO_HI_GIOP_MSGTYPE_CLOSECONNECTION    5
#define __PO_HI_GIOP_MSGTYPE_MESSAGEERROR       6
#define __PO_HI_GIOP_MSGTYPE_FRAGMENT           7

#define __PO_HI_GIOP_VERSION_MAJOR              1
#define __PO_HI_GIOP_VERSION_MINOR              3

#define __PO_HI_GIOP_MAGIC                      "GIOP"

#define __PO_HI_GIOP_OPERATION_MAX_SIZE         100

#define __PO_HI_GIOP_MAGIC_SIZE                 4

#define __PO_HI_GIOP_DISPOSITION_KEY            0
#define __PO_HI_GIOP_DISPOSITION_PROFILE        1
#define __PO_HI_GIOP_DISPOSITION_REFERENCE      2

#define __PO_HI_GIOP_HAS_MORE_MESSAGES          300

typedef struct
{
  char              magic[4];
  struct {
    __po_hi_uint8_t major; /* __PO_HI_GIOP_VERSION_MAJOR */
    __po_hi_uint8_t minor; /* __PO_HI_GIOP_VERSION_MINOR */
  } version;
  __po_hi_uint8_t   flags; 
  __po_hi_uint8_t   message_type;               
  __po_hi_uint32_t  message_size;
}__po_hi_giop_msg_hdr_t;

/* The __po_hi_giop_msg_hdr_t gives a structure to fill a message header */
/* for the GIOP protocol. The flags (8 bits) are organized like this :   */
/*  0  0  0  0  0  0  0  0                                               */
/*                    |  |----- byte order : 1 for little-endian         */
/*                    |                      0 for big-endian            */
/* The message_type should be a value of __PO_HI_GIOP_MSGTYPE*           */

typedef struct
{
  __po_hi_uint16_t               disposition;
  union
  {
    struct
    {
      __po_hi_uint32_t               object_size;
      __po_hi_uint32_t               object_addr;
    }key;
    
    struct
    {
      __po_hi_uint32_t               profile_id;
      __po_hi_uint32_t               profile_length;
      __po_hi_uint32_t               profile_data;
    }profile;
    
    struct
    {
      __po_hi_uint32_t               profile_index;
      __po_hi_uint32_t               full_ior;
    }reference;
  } values;
}__po_hi_giop_request_target_t;
/*
 * Note: for now, we only support object target. The object-id will
 * always be set to 0.
 */

typedef struct
{
        __po_hi_uint32_t               request_id;
        __po_hi_uint8_t                response_flags;
        __po_hi_uint8_t                reserved[3];
        __po_hi_giop_request_target_t  target;
        __po_hi_uint32_t               operation_length;
        char                           operation[__PO_HI_GIOP_OPERATION_MAX_SIZE];
        __po_hi_uint32_t               nb_scontext;
}__po_hi_giop_request_hdr_t;
/*
 * The operation is set the a maximum length of 100
 */

void __po_hi_giop_msg_hdr_init (__po_hi_giop_msg_hdr_t* msg_hdr);
/*
 * Initialize the message header, set the magic number, the version
 * and all other needed variables
 */

void __po_hi_giop_msg_hdr_set_message_type (__po_hi_giop_msg_hdr_t* msg_hdr,
                                            __po_hi_uint8_t msg_type);
/*
 * Set the message type in the header. We only support request, so the
 * type should be always __PO_HI_GIOP_MSG_REQUEST
 */

void __po_hi_giop_msg_hdr_set_message_size (__po_hi_giop_msg_hdr_t* msg_hdr,
                                            __po_hi_uint32_t msg_size);

void __po_hi_giop_request_hdr_init (__po_hi_giop_request_hdr_t* request_hdr);

void __po_hi_giop_request_hdr_set_operation (__po_hi_giop_request_hdr_t* request_hdr,
                                             const char* request_name);

int __po_hi_giop_send (__po_hi_entity_t from,
                       __po_hi_entity_t to,
                       __po_hi_msg_t*   msg);

int __po_hi_giop_decode_msg (__po_hi_msg_t* network_flow, 
                             __po_hi_msg_t* output_msg,
                             __po_hi_uint32_t* has_more);

#ifdef __PO_HI_DEBUG
void __po_hi_giop_print_msg( __po_hi_msg_t* msg);
#endif

#endif /* __PO_HI_GIOP_H__ */

#endif /* __PO_HI_USE_GIOP */
