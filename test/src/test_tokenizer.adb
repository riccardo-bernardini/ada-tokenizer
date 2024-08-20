--
with Tokenize;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada;

procedure Test_Tokenizer is
   Bad_Command_Line : exception;

   package CL is
      function N_Arguments return Natural;

      function First (Default : String) return String;

      function First return String
        with
          Pre => N_Arguments > 0;

      pragma Warnings (Off, "postcondition does not mention function result");

      function Shift return String
        with
          Pre => N_Arguments > 0,
          Post => N_Arguments = N_Arguments'Old - 1;

      procedure Shift
        with
          Pre => N_Arguments > 0,
          Post => N_Arguments = N_Arguments'Old - 1;

      pragma Warnings (On, "postcondition does not mention function result");

   end CL;

   package body Cl is
      First_Unread : Positive := 1;

      function N_Arguments return Natural
      is (Command_Line.Argument_Count - First_Unread + 1);

      function First return String
      is (Command_Line.Argument (First_Unread));


      function First (Default : String) return String
      is (if N_Arguments = 0 then
             Default
          else
             First);

      function Shift return String
      is
      begin
         return Result : constant String := First do
            Shift;
         end return;
      end Shift;

      procedure Shift is
      begin
         First_Unread := First_Unread + 1;
      end Shift;
   end Cl;

   procedure Print_Help
   is
   begin
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("   test_tokenizer [-q] collate_option string [string...]");
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
      Put_Line ("option -q makes the output terse");
      New_Line;
   end Print_Help;

   procedure Print (X : Tokenize.Token_Array) is
   begin
      for I in X'Range loop
         Put ("[" & To_String (X(I)) & "]");

         if I < X'Last then
            Put (" ");
         end if;
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

   Verbose : Boolean := True;
begin
   if Cl.First (Default => "") = "-q" then
      Cl.Shift;
      Verbose := False;
   end if;

   if Cl.N_Arguments < 2 then
      raise Bad_Command_Line;
   end if;

   declare
      Opt : constant Tokenize.Collation_Option := Parse (Cl.Shift);
   begin
      while Cl.N_Arguments > 0 loop
         declare
            Arg : constant String := Cl.Shift;
         begin
            if Verbose then
               Put_Line ("### PARSING ###" & Arg);
            end if;

            Print (Tokenize.Split
                   (To_Be_Splitted    => Arg,
                    Separator         => Maps.To_Set (" ,"),
                    Collation         => Opt));
            New_Line;

            if Verbose then
               Print (Tokenize.Split
                      (To_Be_Splitted     => Arg,
                       Separator          => Maps.To_Set (" ,"),
                       Collate_Separator  => True));
               New_Line;

               Print (Tokenize.Split
                      (To_Be_Splitted     => Arg,
                       Separator          => Maps.To_Set (" ,"),
                       Collate_Separator  => False));
               New_Line;
            end if;
         end;
      end loop;
   end;
exception
   when Bad_Command_Line =>
      Print_Help;

      Command_Line.Set_Exit_Status (Command_Line.Failure);


end Test_Tokenizer;
