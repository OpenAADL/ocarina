with Output; use Output;
with Ocarina.Configuration; use Ocarina.Configuration;

package body Ocarina.Utils is

   -------------
   -- Version --
   -------------

   procedure Version is
   begin
      Write_Line
        ("Ocarina " & Ocarina_Version
           & " (" & Ocarina_SVN_Revision & ")");

      if Ocarina_Last_Configure_Date /= "" then
         Write_Line ("Build date: " & Ocarina_Last_Configure_Date);
      end if;

      Write_Line
        ("Copyright (c) 2003-2009 Telecom ParisTech, 2010-"
           & Ocarina_Last_Configure_Year & " ESA & ISAE");
   end Version;

end Ocarina.Utils;
