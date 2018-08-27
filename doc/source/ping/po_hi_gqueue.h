/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2017 ESA & ISAE.
 */

#ifndef __PO_HI_GQUEUE_H__
#define __PO_HI_GQUEUE_H__

#define __PO_HI_GQUEUE_FULL      10

#define __PO_HI_GQUEUE_FIFO_INDATA    -1
#define __PO_HI_GQUEUE_FIFO_OUT       -2

#define __PO_HI_GQUEUE_INVALID_PORT invalid_port_t
#define __PO_HI_GQUEUE_INVALID_LOCAL_PORT invalid_local_port_t

#include <deployment.h>
#include <request.h>
#include <po_hi_types.h>

/**
 * \brief Initialize a global queue.
 *
 * In a distributed system, each task has
 * its own global queue. This function is invoked by each thead to
 * create its global queue, according to its information (number of
 * ports, destination of each port ...).

 * \param id id of the task associated to this queue.
 * \param nb_ports number of ports for task 'id'.
 * \param queue XXX.
 * \param sizes size of the FIFO for each port, or __PO_HI_GQUEUE_FIFO_OUT if this is an out port.
 * \param first XXX.
 * \param offsets offset position for each queue in the global queue.
 * \param woffsets 
 * \param n_dest number of destinations for each port.
 * \param destinations destination for each port.
 * \param used_size  XXX.
 * \param history  XXX.
 * \param recent  XXX.
 * \param empties  XXX.
 * \param total_fifo_sizes XXX.
 */
void __po_hi_gqueue_init (__po_hi_task_id       id,
			  __po_hi_port_id_t     nb_ports,
			  __po_hi_request_t     queue[],
			  __po_hi_port_id_t     sizes[],
			  __po_hi_port_id_t     first[],
			  __po_hi_port_id_t     offsets[],
			  __po_hi_port_id_t     woffsets[],
			  __po_hi_port_id_t     n_dest[],
			  __po_hi_port_t*       destinations[],
			  __po_hi_port_id_t     used_size[],
			  __po_hi_local_port_t  history[],
			  __po_hi_request_t     recent[],
			  __po_hi_port_id_t     empties[],
			  __po_hi_uint32_t      total_fifo_size);



/**
 * \brief Store a value for an OUT port.
 * 
 * \param id task-id which owns the global queue.
 * \param port port that store the value (local).
 * \param request pointer towards the request to store in the queue.
 */
void __po_hi_gqueue_store_out (__po_hi_task_id id,
                               __po_hi_local_port_t port,
                               __po_hi_request_t* request);



/*
 * \brief Send a value for an out port.
 * 
 * \param id task-id which has the global queue.
 * \param port number of the port that will send the data.
 * \param request pointer towards the request to store in the queue.
 */
/*
int __po_hi_gqueue_send_output (__po_hi_task_id id,
                                 __po_hi_port_t port);
*/


/**
 * \brief Get the value on the specified port.
 * 
 * If the port is an output, this function will return nothing,
 * but will not produce an error. 
 * 
 * If the port is an *IN* event port, this function will return 
 * the last value received in the request parameter, or block until an event arrives.
 * 
 * \param id task-id which owns the global queue.
 * \param port number of port that received the data.
 * \param request pointer to store the received data.
 * \return 0 if there is no error in the assert.
 */
int __po_hi_gqueue_get_value(__po_hi_task_id id,
			     __po_hi_local_port_t port,
			     __po_hi_request_t* request);

/**
 * \brief Dequeue the value on a port.
 * 
 * This function should not be called several times, until
 * you know what you do.
 * 
 * \param id task-id in the local process.
 * \param port port number.
 * \return __PO_HI_SUCCESS if there is no error in the assert.
 */
int __po_hi_gqueue_next_value(__po_hi_task_id id,
			      __po_hi_local_port_t port);

/**
 * \brief Return the number of events that are pending of a port.
 * 
 * \param id task-identifier in the local process.
 * \param port port identifier (or port number) for the thread.
 * \return the number of events that are pending of a port.
 */
int __po_hi_gqueue_get_count(__po_hi_task_id id,
			     __po_hi_local_port_t port);

/**
 * \brief Wait until an event is received on any port for a given thread.
 * 
 * When the function returns, the port argument will contrain the port-id that received the event.
 * 
 * \param id thread identifier in the local process.
 * \param port pointer to a port value.
 */
void __po_hi_gqueue_wait_for_incoming_event(__po_hi_task_id id,
					    __po_hi_local_port_t* port);

/**
 * \brief Store a value in a IN port.
 * 
 * The request argument contrains the request that will be stored in the queue.
 * 
 * \param id task identifier in the local process.
 * \param port port identifier for the local thread.
 * \param request pointer towards what will be stored in the queue.
 * \return the number of events that are pending of a port.
 */
__po_hi_port_id_t __po_hi_gqueue_store_in (__po_hi_task_id id,
					 __po_hi_local_port_t port,
					 __po_hi_request_t* request);

/**
 * \brief Access the most recent value queued.
 *  
 * The program fetches the most recent value on this port in the __po_hi_gqueue_get_most_recent_value array. 
 * It gives the result in the form of a request.
 * WARNING the function doesn't take into account whether the port is an output or input, if the port is empty or not.
 * For this details, see the function get_value.
 * 
 * \param task_id task identifier in the local process.
 * \param local_port port identifier for the local thread.
 * \return the request.
 */
__po_hi_request_t*  __po_hi_gqueue_get_most_recent_value
         (const __po_hi_task_id task_id,
          const __po_hi_local_port_t local_port);

/**
 * \brief Access the destination port thanks to the destination number.
 *  
 * The program fetches the destination port.
 * \param task_id task identifier in the local process.
 * \param local_port port identifier for the local thread.
 * \param destination_number the number of the destination (__po_hi_gqueue_get_destinations_number function).
 * \return the port.
 */
__po_hi_port_t __po_hi_gqueue_get_destination (const __po_hi_task_id task_id,
                                               const __po_hi_local_port_t local_port,
                                               const uint8_t destination_number);

/**
 * \brief Access the destination number (for a specified port).
 *  
 * The program fetches the destination number in the __po_hi_gqueues_n_destinations array.
 * It gives the destination number in the form of a __po_hi_port_id_t.
 * It can be used then to get the destination port with the get_destination function.
 * 
 * \param task_id task identifier in the local process.
 * \param local_port port identifier for the local thread.
 * \return the number.
 */
__po_hi_port_id_t __po_hi_gqueue_get_destinations_number (const __po_hi_task_id task_id,
                                                const __po_hi_local_port_t local_port);


/**
 * \brief Access the size of a port. 
 * 
 * \param id task identifier in the local process.
 * \param port port identifier for the local thread.
 * \return size of port.
 */
__po_hi_port_id_t __po_hi_gqueue_get_port_size(const __po_hi_task_id id,
                                            const __po_hi_local_port_t port);

/**
 * \brief Access the used size of a port. 
 * 
 * \param id task identifier in the local process.
 * \param port port identifier for the local thread.
 * \return size of port.
 */
__po_hi_port_id_t __po_hi_gqueue_used_size( __po_hi_task_id id, __po_hi_local_port_t port);

/**
 * \brief Check whether the queue belonging to the id task is empty.
 *  
 * The program checks the array __po_hi_gqueues_queue_is_empty.
 * \param id task identifier in the local process.
 * \return the value in the array.
 * \return 0 and 1 if the queue is not empty because of array construction.
 */
__po_hi_port_id_t  po_hi_gqueues_queue_is_empty(__po_hi_task_id id);

#endif /* __PO_HI_GQUEUE_H__ */
