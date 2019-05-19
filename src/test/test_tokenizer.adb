--
with Tokenize;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada;

procedure Test_Tokenizer is


   procedure Print (X : Tokenize.Token_List) is
   begin
      for Item of X loop
         Put ("[" & Item & "] ");
      end loop;
   end Print;

begin
   for I in 1 .. Command_Line.Argument_Count loop
      Put_Line ("### PARSING ###" & Command_Line.Argument (I));
      Print (Tokenize.Split
             (To_Be_Splitted    => Command_Line.Argument (I),
              Separator         => Maps.To_Set(" ,"),
              Collate_Separator => True));
      New_Line;
   end loop;
end Test_Tokenizer;
