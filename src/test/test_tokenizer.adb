--
with Tokenize;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada;

procedure Test_Tokenizer is
   Bad_Command_Line : exception;

   procedure Print_Help
   is
   begin
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("   test_tokenizer collate_option string [string...]");
      New_Line;
      Put_Line ("collate_option has one or more of the following letters:");
      New_Line;
      Put_Line ("    N (none)");
      Put_Line ("    H (head)");
      Put_Line ("    T (tail)");
      Put_Line ("    M (middle)");
      Put_Line ("    F (full)");
      New_Line;
      Put_Line ("collate_option is case insensitive");
      New_Line;
   end Print_Help;

   procedure Print (X : Tokenize.Token_Array) is
   begin
      for Item of X loop
         Put ("[" & To_String (Item) & "] ");
      end loop;
   end Print;

   function Parse (S : String) return Tokenize.Collation_Option
   is
      use Tokenize;

      Result : Collation_Option := None;
   begin
      for K in S'Range loop
         case S (K) is
            when 'n' | 'N' =>
               null;

            when 'h' | 'H' =>
               Result := Result and Tokenize.Head;

            when 't' | 'T' =>
               Result := Result and Tokenize.Tail;

            when 'm' | 'M' =>
               Result := Result and Middle;

            when 'f' | 'F' =>
               Result := Full;

            when others =>
               raise Bad_Command_Line;
         end case;
      end loop;

      return Result;
   end Parse;

begin
   if Command_Line.Argument_Count < 2 then
      raise Bad_Command_Line;
   end if;

   declare
      Opt : constant Tokenize.Collation_Option := Parse (Command_Line.Argument (1));
   begin
      for I in 2 .. Command_Line.Argument_Count loop
         Put_Line ("### PARSING ###" & Command_Line.Argument (I));
         Print (Tokenize.Split
                (To_Be_Splitted    => Command_Line.Argument (I),
                 Separator         => Maps.To_Set (" ,"),
                 Collation         => Opt));
         New_Line;
         Print (Tokenize.Split
                (To_Be_Splitted    => Command_Line.Argument (I),
                 Separator         => Maps.To_Set (" ,"),
                 Collate_Separator  => True));
         New_Line;
         Print (Tokenize.Split
                (To_Be_Splitted    => Command_Line.Argument (I),
                 Separator         => Maps.To_Set (" ,"),
                 Collate_Separator  => False));
         New_Line;
      end loop;
   end;
exception
   when Bad_Command_Line =>
      Print_Help;

      Command_Line.Set_Exit_Status (Command_Line.Failure);


end Test_Tokenizer;
