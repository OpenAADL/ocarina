#include <po_hi_protected.h>
#include <subprograms.h>
/*********************/
/* pos_impl_update */ 
/*********************/

void pos_impl_update 
      (pos_impl* value)
{

   __po_hi_protected_lock(value->protected_id);
   update(&(value->field));
   __po_hi_protected_unlock(value->protected_id);
}

/*******************/
/* pos_impl_read */ 
/*******************/

void pos_impl_read 
      (pos_impl* value)
{

   __po_hi_protected_lock(value->protected_id);
   read(&(value->field));
   __po_hi_protected_unlock(value->protected_id);
}


