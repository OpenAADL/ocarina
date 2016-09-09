module lib/property

open meta/contract

/*
Library of Properties that can be analyzed
*/

one sig  	uniprocessor_architectures extends Property{
}

one sig  	periodic_tasks, 
				periodic_tasks_with_jitter, 
				simultaneous_tasks, 
				tasks_with_implicit_deadlines, 
				tasks_with_arbitrary_deadlines, 
				tasks_with_fixed_priority, 
				independent_tasks, 
				tasks_with_bounded_execution_times, 
				preemtible_tasks extends Property{
}

one sig  	periodic_frames, 
				periodic_frames_with_jitter, 
				simultaneous_fames, 
				frames_with_fixed_priority, 
				point_to_point_physical_connections, 
				messages_with_bounded_size, 
				asynchronous_nodes, 
				full_duplex_links,
				static_routing extends Property{
}

one sig  is_schedulable extends Property{
}

one sig arinc653_context, can_context extends Property{}
