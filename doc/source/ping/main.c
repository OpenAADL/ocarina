#include <activity.h>
#include <po_hi_common.h>
#include <po_hi_main.h>
#include <po_hi_time.h>
#include <po_hi_task.h>
/***********************/
/* __PO_HI_MAIN_NAME */ 
/***********************/

__PO_HI_MAIN_TYPE __PO_HI_MAIN_NAME ()
{

   __po_hi_initialize();
   __po_hi_create_periodic_task(node_a_pinger_k,__po_hi_milliseconds(5000),2,pinger_job);
   __po_hi_wait_initialization();
   __po_hi_wait_for_tasks();
   return (__PO_HI_MAIN_RETURN);
}


