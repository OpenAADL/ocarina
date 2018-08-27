#include <po_hi_types.h>
#include <po_hi_gqueue.h>
#include <request.h>
#include <deployment.h>
#include <types.h>
#include <subprograms.h>
#include <po_hi_task.h>
#include <po_hi_main.h>
#include <marshallers.h>
extern __po_hi_entity_t __po_hi_port_global_to_entity[__PO_HI_NB_PORTS];
extern __po_hi_port_t __po_hi_port_global_to_local[__PO_HI_NB_PORTS];
__po_hi_int8_t __po_hi_data_source_local_destinations[1] = {ping_me_global_data_sink};
__po_hi_uint8_t __po_hi_pinger_woffsets[__po_hi_pinger_nb_ports];
__po_hi_uint8_t __po_hi_pinger_offsets[__po_hi_pinger_nb_ports];
__po_hi_uint8_t __po_hi_pinger_used_size[__po_hi_pinger_nb_ports];
__po_hi_uint8_t __po_hi_pinger_empties[__po_hi_pinger_nb_ports];
__po_hi_uint8_t __po_hi_pinger_first[__po_hi_pinger_nb_ports];
__po_hi_uint8_t __po_hi_pinger_recent[__po_hi_pinger_nb_ports * sizeof(__po_hi_request_t)];
__po_hi_uint8_t __po_hi_pinger_queue[0 * sizeof(__po_hi_request_t)];
__po_hi_uint16_t __po_hi_pinger_total_fifo_size = 0;
__po_hi_port_t __po_hi_pinger_history[0];
__po_hi_uint8_t __po_hi_pinger_n_dest[__po_hi_pinger_nb_ports] = {1};
__po_hi_int8_t __po_hi_pinger_fifo_size[__po_hi_pinger_nb_ports] = {__PO_HI_GQUEUE_FIFO_OUT};
__po_hi_uint8_t* __po_hi_pinger_destinations[__po_hi_pinger_nb_ports] = {__po_hi_data_source_local_destinations};
/*  Periodic task : Pinger*/

/****************/
/* pinger_job   */ 
/****************/

void* pinger_job ()
{
   simple_type data_source_request_var;
   __po_hi_request_t data_source_request;

   __po_hi_gqueue_init(node_a_pinger_k,__po_hi_pinger_nb_ports,__po_hi_pinger_queue,__po_hi_pinger_fifo_size,__po_hi_pinger_first,__po_hi_pinger_offsets,__po_hi_pinger_woffsets,__po_hi_pinger_n_dest,__po_hi_pinger_destinations,__po_hi_pinger_used_size,__po_hi_pinger_history,__po_hi_pinger_recent,__po_hi_pinger_empties,__po_hi_pinger_total_fifo_size);
   __po_hi_wait_initialization();
   while (1)
   {
      /*  Call implementation*/
      do_ping_spg(&(data_source_request_var));
      /*  Set the OUT port values*/
      data_source_request.vars.pinger_global_data_source.pinger_global_data_source = data_source_request_var;
      data_source_request.port = data_source_request_var;
      __po_hi_gqueue_store_out(node_a_pinger_k,pinger_local_data_source,&(data_source_request));
      /*  Send the OUT ports*/
      __po_hi_gqueue_send_output(node_a_pinger_k,pinger_global_data_source);
      __po_hi_wait_for_next_period(node_a_pinger_k);
   }
}


/**************************/
/* __po_hi_main_deliver   */ 
/**************************/

void __po_hi_main_deliver 
      (__po_hi_msg_t* message)
{
   __po_hi_request_t request;
   __po_hi_entity_t entity;

   __po_hi_unmarshall_request(&(request),message);
   entity = __po_hi_port_global_to_entity[request.port];
   switch (entity)
   {
      default:
      {
         break;
      }
   }
}


