#include <types.h>
#include <po_hi_types.h>
#include <po_hi_marshallers.h>

/***************************************/
/* __po_hi_marshall_type_simple_type   */ 
/***************************************/

void __po_hi_marshall_type_simple_type 
      (simple_type value,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_marshall_int(value,message,offset);
}


/*****************************************/
/* __po_hi_unmarshall_type_simple_type   */ 
/*****************************************/

void __po_hi_unmarshall_type_simple_type 
      (simple_type* value,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_unmarshall_int(value,message,offset);
}


/************************************************/
/* __po_hi_marshall_request_ping_me_data_sink   */ 
/************************************************/

void __po_hi_marshall_request_ping_me_data_sink 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_marshall_type_simple_type(request->vars.ping_me_global_data_sink.ping_me_global_data_sink,message,offset);
}


/**************************************************/
/* __po_hi_unmarshall_request_ping_me_data_sink   */ 
/**************************************************/

void __po_hi_unmarshall_request_ping_me_data_sink 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_unmarshall_type_simple_type(&(request->vars.ping_me_global_data_sink.ping_me_global_data_sink),message,offset);
}


/*************************************************/
/* __po_hi_marshall_request_pinger_data_source   */ 
/*************************************************/

void __po_hi_marshall_request_pinger_data_source 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_marshall_type_simple_type(request->vars.pinger_global_data_source.pinger_global_data_source,message,offset);
}


/***************************************************/
/* __po_hi_unmarshall_request_pinger_data_source   */ 
/***************************************************/

void __po_hi_unmarshall_request_pinger_data_source 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message,
      __po_hi_uint16_t* offset)
{

   __po_hi_unmarshall_type_simple_type(&(request->vars.pinger_global_data_source.pinger_global_data_source),message,offset);
}


/******************************/
/* __po_hi_marshall_request   */ 
/******************************/

void __po_hi_marshall_request 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message)
{
   __po_hi_uint16_t offset;

   offset = 0;
   __po_hi_marshall_port(request->port,message);
   switch (request->port)
   {
      case ping_me_global_data_sink:
      {
         __po_hi_marshall_request_ping_me_data_sink(request,message,&(offset));

         break;
      }
      case pinger_global_data_source:
      {
         __po_hi_marshall_request_pinger_data_source(request,message,&(offset));

         break;
      }
      default:
      {
         break;
      }
   }
}


/********************************/
/* __po_hi_unmarshall_request   */ 
/********************************/

void __po_hi_unmarshall_request 
      (__po_hi_request_t* request,
      __po_hi_msg_t* message)
{
   __po_hi_uint16_t offset;

   offset = 0;
   __po_hi_unmarshall_port(&(request->port),message);
   switch (request->port)
   {
      case ping_me_global_data_sink:
      {
         __po_hi_unmarshall_request_ping_me_data_sink(request,message,&(offset));

         break;
      }
      case pinger_global_data_source:
      {
         __po_hi_unmarshall_request_pinger_data_source(request,message,&(offset));

         break;
      }
      default:
      {
         break;
      }
   }
}


