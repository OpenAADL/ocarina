/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2017 ESA & ISAE.
 */

#ifndef __PO_HI_MESSAGES_H_
#define __PO_HI_MESSAGES_H_

#include <po_hi_config.h>
#include <po_hi_types.h>

#include <request.h>
/* This file may not be generated. However, using messages implies
   using request. */

#ifdef __PO_HI_USE_GIOP
#define __PO_HI_MESSAGES_MAX_SIZE (int) sizeof(__po_hi_request_t) + 200
#else
#define __PO_HI_MESSAGES_MAX_SIZE (int) sizeof(__po_hi_request_t) + 4
/* XXX Why + 4 ? to be investigated */
#endif

#define __PO_HI_MESSAGES_CONTENT_BIGENDIAN      1
#define __PO_HI_MESSAGES_CONTENT_LITTLEENDIAN   2

typedef struct
{
  __po_hi_uint8_t   content[__PO_HI_MESSAGES_MAX_SIZE]; /* Content of the message */
  __po_hi_uint32_t  length;
  __po_hi_uint8_t   flags;
} __po_hi_msg_t;


void __po_hi_msg_reallocate (__po_hi_msg_t* message);
/*
 * Reset the message given in parameter
 */

void __po_hi_msg_write (__po_hi_msg_t*  msg,
			void*           data,
			__po_hi_uint32_t len);
/*
 * Write the data at the beginning of the specified message.  Length
 * of the data are specified by the parameter len
 */


int __po_hi_msg_length (__po_hi_msg_t* msg);
/*
 * Return the length is the message
 */

void __po_hi_msg_copy (__po_hi_msg_t* dest,
		       __po_hi_msg_t* src);
/*
 * Copy a message. The first argument is the message destination
 * whereas the second argument is the message source
 */

void __po_hi_msg_append_data (__po_hi_msg_t* msg, void* data, __po_hi_uint32_t length);
/*
 * Append data to a message. The first argument is the message which
 * will contain all the data. The second argument is a pointer to the
 * data and the third argument (length) is the size of the data in
 * byte.
 */

void __po_hi_msg_append_msg (__po_hi_msg_t* dest, __po_hi_msg_t* source);
/*
 * Append a message to another message. The first argument is the
 * message in which we will append the data. The second argument is
 * the source of the data.
 */

void __po_hi_msg_get_data (void* dest, __po_hi_msg_t* source,
                           __po_hi_uint32_t index,
                           __po_hi_uint32_t size);
/*
 * Get data from a message at index 'index', and copy it to the dest
 * argument It will copy size bytes from the messages.
 */

void __po_hi_msg_move (__po_hi_msg_t* msg, __po_hi_uint32_t length);
/*
 * Move a part of the message to the beginning. This function will put
 * the part (starting from the length argument) to the beginning of
 * the message.
 */

int __po_hi_msg_should_swap (__po_hi_msg_t* msg);
/*
 * The __po_hi_msg_should_swap return 1 if the endianness of the
 * current processor differs with the endianness of the message. Else,
 * it returns 0.
 */

void __po_hi_msg_swap_value (void* from, void* dest, __po_hi_uint32_t size);
/*
 * The function __po_hi_msg_swap_value swap the bytes of the from
 * value and put it to the dest argument. The size of the value is
 * designed by the third argument.
 */

#ifdef __PO_HI_DEBUG
void __po_hi_messages_debug (__po_hi_msg_t* msg);
#endif

#endif /* __PO_HI_MESSAGES_H_ */
