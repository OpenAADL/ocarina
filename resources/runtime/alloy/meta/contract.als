module meta/contract

/*********************************************************

													Alloy module
*********************************************************/

/*Basic structures manipulated in the Alloy specification*/

/*Relationships with Data and Properties can be specified in a contract*/ 
abstract sig Data_Structure {}
abstract sig Component {
	subcomponents: set Component,
	type: lone Data_Structure,
	properties: set Data_Structure
}
abstract sig Property {}

/*Definition of the structure of a contract*/
abstract sig Contract{
	input: set Component,								//required-provided data 
	output:set Component,								 
	assumption: set Property,				//required-provided properties
	guarantee: set Property,
	// inter-dependencies with other contracts
	nextHoriz:set Contract,				// output->input
    nextVertical:set Contract				// guarantee->assumption	
}

/*
Constraints to be held by satisfiable instance(s) 
*/

//Fact defining an horizontal precedence between two elements
fact HorizontalPrecedence{
    all c_current:Contract | 
		c_current.nextHoriz={c_next:Contract| 
			(c_current.output & c_next.input != none) and
			(all a :c_current.assumption| a in Contract.guarantee) and 
			(all a :c_next.assumption| a in Contract.guarantee) //and 
//			c_current.output != none
	}
}


//Fact defining a vertical precedence between two elements
fact VerticalPrecedence{
    all c_current:Contract | 
		c_current.nextVertical={c_next:Contract|
			(c_current.guarantee & c_next.assumption != none)
	}
}
