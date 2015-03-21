package Ocarina.Backends.Vxworks653_Conf is

--  This backend generates the XML file useful for ARINC653 implementation

   procedure Generate (AADL_Root : Node_Id);
   --  The main entry point of the ARINC653 configuration generator

   procedure Init;
   --  Fills the corresponding location in the generator table by the
   --  information on this generator and execute some initialization
   --  routines necessary for its work.

   procedure Reset;

   procedure Visit_Architecture_Instance (E : Node_Id);

   function Get_XML_Root return Node_Id;

private
   XML_Root                : Node_Id;
   Current_XML_Node        : Node_Id;
   Distributed_Application : Node_Id;
   HI_Node                 : Node_Id;
   HI_Unit                 : Node_Id;
   --  The root of the XML trees

end Ocarina.Backends.Vxworks653_Conf;
