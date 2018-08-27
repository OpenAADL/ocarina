#ifndef __DEPLOYMENT_H_
#define __DEPLOYMENT_H_ 
#include <po_hi_protected.h>
typedef enum
{
   pinger_local_data_source = 0
} __po_hi_pinger_t;

#define __po_hi_pinger_nb_ports 1

typedef enum
{
   ping_me_local_data_sink = 0
} __po_hi_ping_me_t;

#define __po_hi_ping_me_nb_ports 1

/*  For each node in the distributed application add an enumerator*/

typedef enum
{
   node_a_k = 0,
   node_b_k = 1
} __po_hi_node_t;

/*  For each thread in the distributed application nodes, add an enumerator*/

typedef enum
{
   node_a_pinger_k_entity = 0,
   node_b_ping_me_k_entity = 1
} __po_hi_entity_t;

typedef enum
{
   node_a_pinger_k = 0
} __po_hi_task_id;

#define __PO_HI_NB_TASKS 1

/*  For each thread in the distributed application nodes THAT MAY COMMUNICATE*/
/*   with the current node, add an enumerator*/

typedef enum
{
   invalid_server = -1
} __po_hi_entity_server_t;

#define __PO_HI_NB_SERVERS 0

#define __PO_HI_NB_PROTECTED 0

#define __PO_HI_NB_NODES 2

#define __PO_HI_NB_ENTITIES 2

#define __PO_HI_NB_PORTS 2

typedef enum
{
   pinger_global_data_source = 0,
   ping_me_global_data_sink = 1
} __po_hi_port_t;

#endif
