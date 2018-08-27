#ifndef __TYPES_H_
#define __TYPES_H_ 
#include <po_hi_types.h>
#include <po_hi_protected.h>
typedef int pos_internal_type;

typedef struct
{
   __po_hi_protected_t protected_id;

   pos_internal_type field;

} pos_impl;

void pos_impl_update 
      (pos_impl* value);

void pos_impl_read 
      (pos_impl* value);

#endif
