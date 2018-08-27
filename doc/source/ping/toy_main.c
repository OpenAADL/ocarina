#include <activity.h>
#include <po_hi_common.h>
#include <po_hi_main.h>
#include <po_hi_time.h>
#include <po_hi_task.h>
#include <types.h>
extern pos_impl pos_data;
/***********************/
/* __PO_HI_MAIN_NAME */ 
/***********************/

__PO_HI_MAIN_TYPE __PO_HI_MAIN_NAME ()
{

   __po_hi_initialize();
   __po_hi_create_periodic_task(gnc_tmtc_pos_gnc_th_k,__po_hi_milliseconds(1000),250,gnc_th_job);
   __po_hi_create_periodic_task(gnc_tmtc_pos_tmtc_th_k,__po_hi_milliseconds(100),190,tmtc_th_job);
   pos_data.protected_id = 0;
   __po_hi_wait_initialization();
   __po_hi_wait_for_tasks();
   return (__PO_HI_MAIN_RETURN);
}


