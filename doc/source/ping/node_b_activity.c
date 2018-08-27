#include <po_hi_gqueue.h>
#include <po_hi_types.h>
#include <request.h>
#include <deployment.h>
#include <po_hi_task.h>
#include <subprograms.h>
#include <po_hi_main.h>
#include <marshallers.h>
extern __po_hi_entity_t __po_hi_port_global_to_entity[__PO_HI_NB_PORTS];
extern __po_hi_port_t __po_hi_port_global_to_local[__PO_HI_NB_PORTS];
__po_hi_uint8_t __po_hi_ping_me_woffsets[__po_hi_ping_me_nb_ports];
__po_hi_uint8_t __po_hi_ping_me_offsets[__po_hi_ping_me_nb_ports];
__po_hi_uint8_t __po_hi_ping_me_used_size[__po_hi_ping_me_nb_ports];
__po_hi_uint8_t __po_hi_ping_me_empties[__po_hi_ping_me_nb_ports];
__po_hi_uint8_t __po_hi_ping_me_first[__po_hi_ping_me_nb_ports];
__po_hi_uint8_t __po_hi_ping_me_recent[__po_hi_ping_me_nb_ports * sizeof(__po_hi_request_t)];
__po_hi_uint8_t __po_hi_ping_me_queue[16 * sizeof(__po_hi_request_t)];
__po_hi_uint16_t __po_hi_ping_me_total_fifo_size = 16;
__po_hi_port_t __po_hi_ping_me_history[16];
__po_hi_uint8_t __po_hi_ping_me_n_dest[__po_hi_ping_me_nb_ports] = {0};
__po_hi_int8_t __po_hi_ping_me_fifo_size[__po_hi_ping_me_nb_ports] = {16};
__po_hi_uint8_t* __po_hi_ping_me_destinations[__po_hi_ping_me_nb_ports] = {NULL};

/*********************/
/* ping_me_deliver   */ 
/*********************/

void ping_me_deliver 
      (__po_hi_request_t* request)
{

   switch (request->port)
   {
      case ping_me_global_data_sink:
      {
         __po_hi_gqueue_store_in(node_b_ping_me_k,ping_me_local_data_sink,request);

         break;
      }
      default:
      {
         break;
      }
   }
}

/*  Sporadic task : Ping_Me*/
/*  Get the IN ports values*/

/*****************/
/* ping_me_job   */ 
/*****************/

void* ping_me_job ()
{
   __po_hi_port_t port;
   __po_hi_request_t data_sink_request;

   __po_hi_gqueue_init(node_b_ping_me_k,__po_hi_ping_me_nb_ports,__po_hi_ping_me_queue,__po_hi_ping_me_fifo_size,__po_hi_ping_me_first,__po_hi_ping_me_offsets,__po_hi_ping_me_woffsets,__po_hi_ping_me_n_dest,__po_hi_ping_me_destinations,__po_hi_ping_me_used_size,__po_hi_ping_me_history,__po_hi_ping_me_recent,__po_hi_ping_me_empties,__po_hi_ping_me_total_fifo_size);
   __po_hi_wait_initialization();
   while (1)
   {
      __po_hi_gqueue_wait_for_incoming_event(node_b_ping_me_k,&(port));
      __po_hi_compute_next_period(node_b_ping_me_k);
      if (__po_hi_gqueue_get_count(node_b_ping_me_k,ping_me_local_data_sink))
      {
               __po_hi_gqueue_get_value(node_b_ping_me_k,ping_me_local_data_sink,&(data_sink_request));
         __po_hi_gqueue_next_value(node_b_ping_me_k,ping_me_local_data_sink);

      }
      /*  Call implementation*/
      ping_spg(data_sink_request.vars.ping_me_global_data_sink.ping_me_global_data_sink);
      __po_hi_wait_for_next_period(node_b_ping_me_k);
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
      case node_b_ping_me_k_entity:
      {
         ping_me_deliver(&(request));

         break;
      }
      default:
      {
         break;
      }
   }
}


