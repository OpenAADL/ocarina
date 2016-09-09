module lib/data_structure

open lib/property

/*
Library of Data Structures that can be analyzed
*/

//Components
one sig 	system, device, 
				process, thread, data, 
				bus, data_flow,
				virtual_bus, virtual_processor,  
				processor, memory extends Data_Structure{
}

//Thread/Tasks properties
one sig  	dispatch_protocol,
				period, 
				compute_execution_time, 
				offset, 
				priority, 
				deadline, 
				jitter, 
				policy,
//processing/scheduling properties
				processor_properties__max_prio_first,
				preemptive_scheduler,
				scheduling_protocol, 
//Dataflows properties
				afdx_properties__afdx_bandwidth_allocation_gap, 	//period 	 
				afdx_properties__afdx_frame_size,							//max_frame_size 				
//				jitter,
//Networking properties
				afdx_properties__afdx_tx_jitter,									//TxJitterTime
				afdx_properties__afdx_es_delay, 								//TxLatencyTime, RxLatencyTime,
				afdx_properties__afdx_sw_delay,								//SwLatencyTime,
				bus_properties__bandwidth,										//speed
				bus_properties__available_bandwidth,						//speed
				bus_properties__channel_type
extends Data_Structure{
}

one sig 
actual_connection_binding,
actual_memory_binding,
actual_processor_binding,
source_text,
//ARINC 653 properties
arinc653__access_type,
//Deployment properties
deployment__execution_platform
extends Data_Structure{
}

//satellite
one sig 
source_data_size
extends Data_Structure{
}

//mars_pathinder
one sig 
posix_scheduling_policy,
concurrency_control_protocol,
data_sheet__uuid
extends Data_Structure{
}

//ravenscar
one sig
deployment__process_id,
deployment__channel_address,
compute_entrypoint_source_text,
priority_range,
clock_period
extends Data_Structure{
}

//fms
one sig
arinc653__slots_allocation,
arinc653__partition_slots,
arinc653__module_major_frame
extends Data_Structure{
}

one sig response_time extends Data_Structure{}

