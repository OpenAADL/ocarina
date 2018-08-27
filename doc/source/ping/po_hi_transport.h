/*
 * This is a part of PolyORB-HI-C distribution, a minimal
 * middleware written for generated code from AADL models.
 * You should use it with the Ocarina toolsuite.
 *
 * For more informations, please visit http://taste.tuxfamily.org/wiki
 *
 * Copyright (C) 2007-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.
 */

#ifndef __PO_HI_TRANSPORT__
#define __PO_HI_TRANSPORT__

#include <po_hi_messages.h>
#include <deployment.h>
#include <request.h>

#define __PO_HI_BIGENDIAN     0
#define __PO_HI_LITTLEENDIAN  1

#ifndef __PO_HI_NB_PROTOCOLS
#define __PO_HI_NB_PROTOCOLS 0
#endif

typedef struct
{
      void (*marshaller)   (void*, void*, int*);
      void (*unmarshaller) (void*, void*, int);
}__po_hi_protocol_conf_t;


#if __PO_HI_NB_PORTS > 0

typedef int (*__po_hi_transport_sending_func)(__po_hi_task_id, __po_hi_port_t);

typedef uint8_t __po_hi_queue_id;

/**
 * \fn __po_hi_transport_get_n_accessed_buses
 *
 * \brief Return the number of buses associated with a device.
 * If no bus is connected to the device or if the device
 * is invalid, the function returns 0. Otherwise, a positive value is
 * returned.
 */
uint32_t __po_hi_transport_get_n_accessed_buses (const __po_hi_device_id device);

/**
 * \fn __po_hi_transport_get_accessed_buses
 *
 * \brief Return a pointer to an array that contains all buses identifiers
 * accessed by the device passed as argument. If the argument is an invalid
 * device-id or if the device does not access any bus, NULL is returned.
 * The size of the array can be retrieved by the __po_hi_get_n_accessed_buses
 * function.
 */
__po_hi_bus_id* __po_hi_transport_get_accessed_buses (const __po_hi_device_id device);


/**
 * \fn __po_hi_transport_share_bus
 *
 * \brief Returns 1 if two devices share a common bus, 0 otherwise.
 */
int __po_hi_transport_share_bus (const __po_hi_device_id, const __po_hi_device_id);

/**
 * \fn __po_hi_get_node_from_entity
 *
 * \brief Returns the node identifier that corresponds to an entity.
 */
__po_hi_node_t    __po_hi_transport_get_node_from_entity (const __po_hi_entity_t entity);

/*
 * \fn __po_hi_get_entity_from_global_port
 *
 * \brief Return the entity identifier that own the port in parameters.
 */
__po_hi_entity_t  __po_hi_get_entity_from_global_port (const __po_hi_port_t port);

/*
 * \fn            __po_hi_transport_send_default
 * \brief         Default transport layer function.
 */
int               __po_hi_transport_send (__po_hi_task_id id, __po_hi_port_t port);

#define __po_hi_transport_send_default __po_hi_transport_send
#define __po_hi_send_output            __po_hi_transport_send

/*
 * \fn      __po_hi_get_port_name
 * \brief   Return the name of the port similar to the name within the AADL model.
 */
char* __po_hi_get_port_model_name (const __po_hi_port_t port);

/*
 * \fn      __po_hi_get_port_name
 * \brief   Return the name of the port according to mapping rules.
 */
char* __po_hi_get_port_name (const __po_hi_port_t port);

/*
 * \fn      __po_hi_get_local_port_from_local_port
 * \brief   Return the local port identifier of the given global port to handle data on the node.
 */

__po_hi_local_port_t __po_hi_get_local_port_from_global_port (const __po_hi_port_t global_port);

/*
 * \fn      __po_hi_get_endianness
 * \brief   Return the endianness of the node given in parameter.
 *
 * The resulting value is either __PO_HI_BIGENDIAN  or __PO_HI_LITTLEENDIAN.
 */
__po_hi_uint8_t  __po_hi_get_endianness (const __po_hi_node_t node);

/*
 * \fn      __po_hi_get_device_from_port
 * \brief   Return the device associated with a given port.
 *
 * The resulting value is a device identifier generated in deployment.h.
 * If no device is associated with the port, it returns the constant
 * value invalid_device_id.
 */
__po_hi_device_id __po_hi_get_device_from_port (const __po_hi_port_t port);


char* __po_hi_get_device_naming (const __po_hi_device_id dev);

/*
 * \fn      __po_hi_get_device_configuration
 * \brief   Returns a pointer to the configuration data of the device.
 *
 * The configuration data can be either a string of a more complex
 * data structure, such as an instance of an ASN1 type.
 */
void* __po_hi_get_device_configuration (const __po_hi_device_id);


/*
 * \fn      __po_hi_transport_get_data_size
 * \brief   Returns the size of the data stored in the port given as parameter.
 */
__po_hi_uint32_t __po_hi_transport_get_data_size (const __po_hi_port_t portno);


/*
 * \fn      __po_hi_transport_get_queue_size
 * \brief   Return the size of the queue associated with the port.
 *
 * The size if specified as the number of request the port can store,
 * this is NOT the number of bytes that can be stored.
 */
__po_hi_uint32_t __po_hi_transport_get_queue_size (const __po_hi_port_t portno);

/*
 * \fn      __po_hi_transport_get_port_kind
 * \brief   Indicate the kind of the port given in parameter or __PO_HI_INVALID_PORT_KIND when not appropriate.
 *
 * The values that are returned indicate if the port is a pure event
 * port, if it has a data associated and if it is an inter-process
 * port or not.
 *
 * Potential return values are:
 *
 *  __PO_HI_IN_DATA_INTER_PROCESS
 *  __PO_HI_OUT_DATA_INTER_PROCESS
 *  __PO_HI_IN_DATA_INTRA_PROCESS
 *  __PO_HI_OUT_DATA_INTRA_PROCESS
 *  __PO_HI_IN_EVENT_DATA_INTER_PROCESS
 *  __PO_HI_OUT_EVENT_DATA_INTER_PROCESS
 *  __PO_HI_IN_EVENT_DATA_INTRA_PROCESS
 *  __PO_HI_OUT_EVENT_DATA_INTRA_PROCESS
 *  __PO_HI_IN_EVENT_INTER_PROCESS
 *  __PO_HI_OUT_EVENT_INTER_PROCESS
 *  __PO_HI_IN_EVENT_INTRA_PROCESS
 *  __PO_HI_OUT_EVENT_INTRA_PROCESS
 *  __PO_HI_INVALID_PORT_KIND
 */
__po_hi_port_kind_t __po_hi_transport_get_port_kind (const __po_hi_port_t portno);


/*
 * \fn      __po_hi_transport_get_model_name
 * \brief   Return the name of the port given in parameter.
 */

char*             __po_hi_transport_get_model_name (const __po_hi_port_t portno);


/* \fn      __po_hi_transport_get_mynode
 * \brief   Return the node identifier of the node that executes the current system.
 */
__po_hi_node_t    __po_hi_transport_get_mynode (void);

/* \fn      __po_hi_transport_get_node_from_device
 * \brief   Return the node identifier associated with the device given in parameter.
 */
__po_hi_node_t    __po_hi_transport_get_node_from_device (const __po_hi_device_id device);


/* \fn      __po_hi_transport_associate_port_bus
 * \brief   Associate a port to a bus. Return 1 on success, 0 otherwise.
 *
 * When calling this function, you have to be very careful and make sure
 * that the bus you are passing by argument is connected to the node
 * that actually host this port.
 * Definition of port and bus values are enclosed in the deployment.h
 * file generated by Ocarina.
 */

int __po_hi_transport_associate_port_bus (const __po_hi_port_t port, const __po_hi_bus_id bus);

/*
 * \fn      __po_hi_transport_get_protocol
 * \brief   Return the protocol identifier that is used between port src and port dst.
 *
 * Get the protocol identifier used to communicate
 * between port src and port dst. It returns a protocol
 * identifier generated in deployment.h.
 * If no specific protocol is used, it returns the value
 * invalid_protocol.
 */
__po_hi_protocol_t         __po_hi_transport_get_protocol (const __po_hi_port_t src, const __po_hi_port_t dst);

/*
 * \fn      __po_hi_transport_get_protocol_configuration
 * \brief   Retrieve the configuration of the given protocol identifier. Returns a pointer on the conf or NULL.
 *
 *
 * Protocol identifier can be retrieve in the generated deployment.h file
 * under the type __po_hi_protocol_t. Invalid protocol identifier
 * will result in returning NULL.
 */
__po_hi_protocol_conf_t*   __po_hi_transport_get_protocol_configuration (const __po_hi_protocol_t p);


/*
 * \fn      __po_hi_transport_set_sending_func
 * \brief   Set the sending function to be called to send data using a particular device.
 *
 *
 * The first argument is device that would be used to send the data while the
 * second is the function that would be called.
 *
 * The function returns __PO_HI_SUCCESS when the new calling function
 * is successfully set. Otherwise, returns __PO_HI_UNAVAILABLE.
 */
int __po_hi_transport_set_sending_func (const __po_hi_device_id device, const __po_hi_transport_sending_func func);

/*
 * \fn      __po_hi_transport_call_sending_func_by_device
 * \brief   Call the sending function to send data from a port associated to a
 *          task.
 *
 * First argument is the device that would be used to send the data. The second
 * argument is the task that is sending the data while th third argument
 * is the port identifier that contain the data to be sent.
 *
 * The function returns  __PO_HI_UNAVAILABLE is no sending function
 * has been set for this device or if the device identifier is invalid.
 * Otherwise, it returns the value returned by the sending function
 * associated to the device.
 */
int __po_hi_transport_call_sending_func_by_device (const __po_hi_device_id, __po_hi_task_id, __po_hi_port_t);

/*
 * \fn      __po_hi_transport_call_sending_func
 * \brief   Call the sending function to send data from a port associated to a
 *          task. The function to call is retrieved using the port.
 *
 * First argument is the task that is sending the data. The second
 * is the port associated with the task and the device. The device
 * to call is deduced from the port.
 *
 * The function returns  __PO_HI_UNAVAILABLE is no sending function
 * has been set for this device or if the device identifier is invalid.
 * Otherwise, it returns the value returned by the sending function
 * associated to the device.
 */
int __po_hi_transport_call_sending_func_by_port (__po_hi_task_id, __po_hi_port_t);



/*
 * \fn      __po_hi_transport_get_sending_func
 * \brief   Get the sending function to be called to send data using a device
 *
 *
 * The first argument is device that would be used to send the data.
 * Returns NULL if the device identifier is incorrect or no function
 * has been set.
 */

__po_hi_transport_sending_func __po_hi_transport_get_sending_func (const __po_hi_device_id device);

/*
 * These functions map PolyORB-HI/C ports to Xtratum (resp. AIR) ones
 */

#ifdef XM3_RTEMS_MODE
void __po_hi_transport_xtratum_port_init (const __po_hi_port_t portno, int val);
int __po_hi_transport_xtratum_get_port (const __po_hi_port_t portno);
#endif

#ifdef AIR_HYPERVISOR
void __po_hi_transport_air_port_init (const __po_hi_port_t portno, long int val);
long int __po_hi_transport_air_get_port (const __po_hi_port_t portno);
#endif

#endif /* __PO_HI_NB_PORTS > 0 */



#endif /* __PO_HI_TRANSPORT__ */
