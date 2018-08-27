/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.
 */

#ifndef __PO_HI_MARSHALLERS_H_
#define __PO_HI_MARSHALLERS_H_

#include <deployment.h>
#include <request.h>
#include <po_hi_messages.h>

/*
 * Basic marshallers functions. These functions are then
 * reused in the generated code to marshall application data.
 */

void __po_hi_marshall_port (__po_hi_port_t value, __po_hi_msg_t* msg);
void __po_hi_unmarshall_port (__po_hi_port_t* value, __po_hi_msg_t* msg);

void __po_hi_marshall_array (void* value, __po_hi_msg_t* msg,__po_hi_uint32_t size, __po_hi_uint32_t* offset);
void __po_hi_unmarshall_array (void* value, __po_hi_msg_t* msg,__po_hi_uint32_t size, __po_hi_uint32_t* offset);

void __po_hi_marshall_bool (__po_hi_bool_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_bool (__po_hi_bool_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_char (char value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_char (char* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_int (int value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_int (int* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_float (float value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_float (float* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_float32 (__po_hi_float32_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_float32 (__po_hi_float32_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_float64 (__po_hi_float64_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_float64 (__po_hi_float64_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_int8 (__po_hi_int8_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_int8 (__po_hi_int8_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_int16 (__po_hi_int16_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_int16 (__po_hi_int16_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_int32 (__po_hi_int32_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_int32 (__po_hi_int32_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

#ifndef COMPCERT
void __po_hi_marshall_int64 (__po_hi_int64_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_int64 (__po_hi_int64_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
#endif

void __po_hi_marshall_uint8 (__po_hi_uint8_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_uint8 (__po_hi_uint8_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_uint16 (__po_hi_uint16_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_uint16 (__po_hi_uint16_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

void __po_hi_marshall_uint32 (__po_hi_uint32_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_uint32 (__po_hi_uint32_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);

#ifndef COMPCERT
void __po_hi_marshall_uint64 (__po_hi_uint64_t value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
void __po_hi_unmarshall_uint64 (__po_hi_uint64_t* value, __po_hi_msg_t* msg,__po_hi_uint32_t* offset);
#endif

#ifdef PO_HI_USE_ASN1
#include <asn1_deployment.h>
#define __PO_HI_ASN1_PKT_SIZE Pkt_REQUIRED_BYTES_FOR_ENCODING
#define __po_hi_asn1_buffer_t __po_hi_byte_t*
#define __po_hi_asn1_pkt_t    Pkt
#endif

#endif /* __PO_HI_MARSHALLERS_H_ */
