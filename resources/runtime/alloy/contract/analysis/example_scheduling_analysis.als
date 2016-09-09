module contract/analysis/example_scheduling_analysis

open lib/data_structure

/*********************************************************
								Generated from the repository of analyses
*********************************************************/

/* 
Declaration of the contracts representing the analyses present in the repository 
*/

-------------------------------
------- Analyses
------------------------------

one sig FPP_RTA extends Contract{
}{
	assumption=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority) in th.properties and
																	(not (deadline) in th.properties and not (offset) in th.properties)
															)
													)
				}
   guarantee=none
   output={/*sys:Component |  	sys.type=system and 
												 		sys.properties=none and 
												 		sys.subcomponents={procss:Component |		
																										procss.type=process and
																										procss.properties=none and 
																										procss.subcomponents={*/	thrd:Component| 
																																							thrd.type=thread and
																																							thrd.subcomponents=none and 
																																							thrd.properties=response_time
																																						//}
																						//}
				}
}

one sig pseudo_model1 extends Component{}{
			type=thread
			subcomponents=none
			properties=response_time
}

one sig pseudo_model2 extends Component{}{
			type=virtual_bus
			subcomponents=none
			properties=response_time
}

one sig pseudo_model3 extends Component{}{
			type=bus
			subcomponents=none
			properties=response_time
}

/*
one sig module1 extends Component{}{
	type=system
	subcomponents=module1_m1_process
	properties=none
}

one sig module1_m1_process extends Component{}{
	type=process
	subcomponents=module1_m1_process_t1
	properties=none
}

one sig module1_m1_process_t1 extends Component{}{
	type=thread
	subcomponents=none
	properties=response_time
}*/

one sig DC_FPP_RTA extends Contract{
}{
	assumption=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority+deadline) in th.properties and
																	(not (offset) in th.properties)
															)
													)
				}
   guarantee=none
   output={/*sys:Component |  	sys.type=system and 
												 		sys.properties=none and 
												 		sys.subcomponents={procss:Component |		
																										procss.type=process and
																										procss.properties=none and 
																										procss.subcomponents={*/	thrd:Component| 
																																							thrd.type=thread and
																																							thrd.subcomponents=none and 
																																							thrd.properties=response_time
																																						//}
																						//}
				}
				
}

one sig AD_FPP_RTA extends Contract{}{
	assumption=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority+deadline) in th.properties and
																	(not (offset) in th.properties)
															)
													)
				}
   guarantee=none
   output={/*sys:Component |  	sys.type=system and 
												 		sys.properties=none and 
												 		sys.subcomponents={procss:Component |		
																										procss.type=process and
																										procss.properties=none and 
																										procss.subcomponents={*/	thrd:Component| 
																																							thrd.type=thread and
																																							thrd.subcomponents=none and 
																																							thrd.properties=response_time
																																						//}
																						//}
				}
}

one sig O_FPP_RTA extends Contract{
}{
	assumption=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority+deadline+offset) in th.properties
															)
													)
				}
   guarantee=none
   output={/*sys:Component |  	sys.type=system and 
												 		sys.properties=none and 
												 		sys.subcomponents={procss:Component |		
																										procss.type=process and
																										procss.properties=none and 
																										procss.subcomponents={*/	thrd:Component| 
																																							thrd.type=thread and
																																							thrd.subcomponents=none and 
																																							thrd.properties=response_time
																																						//}
																						//}
				}
}

one sig FEASIBILITY_ANALYSIS extends Contract{
}{
	assumption=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority+deadline) in th.properties and
																	(not (offset) in th.properties)
															)
													)
				}
   guarantee=is_schedulable
   output=none
}

one sig ARINC653_RTA extends Contract {}{
	assumption=arinc653_context
	input={S:Component | 		S.type=system and  
												(some sub:S.subcomponents | sub.type =processor and
															(arinc653__slots_allocation+arinc653__partition_slots+arinc653__module_major_frame) in sub.properties and
															virtual_processor in sub.subcomponents.type and
															( let vp=sub.subcomponents & virtual_processor.~type | 
																	(scheduling_protocol) in vp.properties 
															)
												) and
												(some sub:S.subcomponents | sub.type =process)
				}
   guarantee=none
   output={/*sys:Component |  	sys.type=system and 
												 		sys.properties=none and 
												 		sys.subcomponents={procss:Component |		
																										procss.type=process and
																										procss.properties=none and 
																										procss.subcomponents={*/	thrd:Component| 
																																							thrd.type=thread and
																																							thrd.subcomponents=none and 
																																							thrd.properties=response_time
																																						//}
																						//}
				}
}

one sig AFDX_NET_RTA extends Contract{
}{
	assumption=
				periodic_frames+
				periodic_frames_with_jitter+ 
				simultaneous_fames+
				frames_with_fixed_priority+ 
				point_to_point_physical_connections+ 
				messages_with_bounded_size+ 
				asynchronous_nodes+ 
				full_duplex_links+
				static_routing
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =system and 
															(afdx_properties__afdx_es_delay) in sub.properties) and
													(some sub:S.subcomponents | sub.type =device and 
															(afdx_properties__afdx_sw_delay) in sub.properties) and
													(some sub:S.subcomponents | sub.type =bus and
															(bus_properties__bandwidth) in sub.properties and  
															virtual_bus in sub.subcomponents.type and
															(let vl=sub.subcomponents & virtual_bus.~type | 
																	(afdx_properties__afdx_tx_jitter+afdx_properties__afdx_bandwidth_allocation_gap+afdx_properties__afdx_frame_size) in vl.properties 
															)
													)
				}
	guarantee=none
	output=	{	vl:Component| 
									vl.type=virtual_bus and
									vl.subcomponents=none and 
									vl.properties=response_time
					}
}

one sig CAN_NET_RTA extends Contract{
}{
	assumption=
				can_context
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =bus and
															(bus_properties__available_bandwidth) in sub.properties and  
															sub.subcomponents.type=none
													)
				}
	guarantee=none
	output=	{	b:Component| 
									b.type=bus and
									b.subcomponents=none and 
									b.properties=response_time
					}
}

----------------------------------------
----- Meta-properties Analyses
----------------------------------------

one sig FPP_RTA_META extends Contract{
}{
	assumption=none
	input={thrd:Component | 		thrd.type=thread and (some p:thrd.properties | p=response_time)}
	guarantee=is_schedulable
	output=none
}

one sig AFDX_NET_RTA_META extends Contract{
}{
	assumption=none
	input={vl:Component | 		vl.type=virtual_bus and (some p:vl.properties | p=response_time)}
	guarantee=is_schedulable
	output=none
}

one sig CAN_NET_RTA_META extends Contract{
}{
	assumption=none
	input={b:Component | 		b.type=bus and (some p:b.properties | p=response_time)}
	guarantee=is_schedulable
	output=none
}

----------------------------------------
----- Contexts Analyses
----------------------------------------

one sig LL_context_analysis extends Contract{
}{
	assumption= none
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =processor and 
															(scheduling_protocol+preemptive_scheduler) in sub.properties) and
													(some sub:S.subcomponents | sub.type =process and  
															thread in sub.subcomponents.type and
															( let th=sub.subcomponents & thread.~type | 
																	(dispatch_protocol +period +compute_execution_time +priority) in th.properties
															)
													)
				}
 /*	input.type=system 
	input.subcomponents.type=thread 
	input.subcomponents.properties=period+execution_time+priority
	#input=1*/
	guarantee=
			periodic_tasks
			+periodic_tasks_with_jitter
			+simultaneous_tasks
			+tasks_with_implicit_deadlines
			+tasks_with_arbitrary_deadlines
			+tasks_with_fixed_priority
			+independent_tasks
			+tasks_with_bounded_execution_times
			+preemtible_tasks
			+uniprocessor_architectures
	output=none
}

one sig ARINC653_context_analysis extends Contract{
}{
	assumption= none
	input={S:Component | 		S.type=system and  
												(some sub:S.subcomponents | sub.type =processor and
															(arinc653__slots_allocation+arinc653__partition_slots+arinc653__module_major_frame) in sub.properties and
															virtual_processor in sub.subcomponents.type and
															( let vp=sub.subcomponents & virtual_processor.~type | 
																	(scheduling_protocol) in vp.properties 
															)
												) and
												(some sub:S.subcomponents | sub.type =process)
				}
	guarantee=arinc653_context
	output=none
}

one sig AFDX_context_analysis extends Contract{
}{
	assumption= none
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =system and 
															(afdx_properties__afdx_es_delay) in sub.properties) and
													(some sub:S.subcomponents | sub.type =device and 
															(afdx_properties__afdx_sw_delay) in sub.properties) and
													(some sub:S.subcomponents | sub.type =bus and
															(bus_properties__bandwidth) in sub.properties and  
															virtual_bus in sub.subcomponents.type and
															(let vl=sub.subcomponents & virtual_bus.~type | 
																	(afdx_properties__afdx_tx_jitter+afdx_properties__afdx_bandwidth_allocation_gap+afdx_properties__afdx_frame_size) in vl.properties 
															)
													)
				}
	guarantee=
				periodic_frames+
				periodic_frames_with_jitter+ 
				simultaneous_fames+
				frames_with_fixed_priority+ 
				point_to_point_physical_connections+ 
				messages_with_bounded_size+ 
				asynchronous_nodes+ 
				full_duplex_links+
				static_routing
	output=none
}

one sig CAN_context_analysis extends Contract{
}{
	assumption= none
	input={S:Component | 		S.type=system and  
													(some sub:S.subcomponents | sub.type =bus and
															(bus_properties__available_bandwidth) in sub.properties and  
															sub.subcomponents.type=none
													)
				}
	guarantee=can_context
	output=none
}
