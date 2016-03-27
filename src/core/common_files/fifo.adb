package body Fifo is

   ----------
   -- Push --
   ----------

   procedure Push (List : in out Fifo_Type; Item : in Element_Type) is
   begin
      List.Prepend (Item);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Fifo_Type; Item : out Element_Type) is
   begin
      if Is_Empty (List) then
         raise Empty_Error;
      end if;
      Item := List.Last_Element;
      List.Delete_Last;
   end Pop;

   function Is_Empty (List : Fifo_Type) return Boolean is
   begin
      return List_Pkg.Is_Empty (List_Pkg.List (List));
   end Is_Empty;
end Fifo;
