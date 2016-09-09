module main

open contract/analysis/example_scheduling_analysis
open contract/goal/example_scheduling_goal

/*
open contract/model/ravenscar_aadl
open contract/model/dre_aadl
open contract/model/pathfinder_aadl
open contract/model/satellite_aadl
open contract/model/fms_aadl
*/
open con_model

/*
This is the main alloy file to execute with Alloy Analyzer
Default settings for the Alloy analyzer to be adjusted if necessary
*/

fact no_univ_signature{
			Univ=none
}

//adjust resolution scope if necessary
run {} for 1
