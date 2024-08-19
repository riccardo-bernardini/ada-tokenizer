--                              -*- Mode: Ada -*-
--  Filename        : tokenize.ads
--  Description     : Ruby-like split
--  Author          : Finta Tartaruga
--  Created On      : Tue Sep 11 22:05:53 2007
--  Last Modified By: R. Bernardini
--  Last Modified On: November 14, 2007
--  Update Count    : 1
--  Status          : <TESTED>

--
--  This package provides a function Split which divides its input
--  string in smaller strings, separated by a "separator" (much as the
--  split function in Perl, Ruby, and so on...).  Function Split returns
--  a Token_List (defined by this package)  whose elements can be accessed
--  by the function Element.
--
--  Function Split can accept a third Boolean value Collate_Separator.
--  If Collate_Separator is true, consecutive istances of the separator are
--  considered as a single one.  If Collate_Separator is False, for every
--  pair of consecutive separator characters an empty string will be returned.
--  Moreover, if Collate_Separator is True, any separator at the beginning of
--  the string is ignored.  Separators at the end are always ignored.
--
--  The default value of Collate_Separator is true if the separator
--  is the space, false otherwise.
--
--  Examples:
--
--   Split("Hello   there")               returns "Hello" and "there"
--   Split("Hello   there", ' ', False)   returns "Hello", "" and "there"
--   Split("Hello::there", ':')           returns "Hello", "" and "there"
--   Split("Hello::there", ':', True)     returns "Hello" and "there"
--
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Maps;

package Tokenize with SPARK_Mode => On is

   type    Token_Array is array (Positive range <>) of Unbounded_String;

   --
   --  Collation is considering successive instances of separators as
   --  one and/or ignoring separators at the beginning or the end of
   --  the string.  For example, if collation is applied and the separator
   --  is ';' the string
   --
   --         ";foo;;bar;pooh;"
   --
   --  will be splitted to
   --
   --          "foo", "bar" and "pooh"
   --
   --  while if collation is not applied the result will be
   --
   --          "", "foo", "", "bar", "pooh" and ""
   --
   --  A variable of type Collation_Option controls which kind of
   --  collation is applied
   --
   type Collation_Option is private;

   None   : constant Collation_Option; -- No collation
   Head   : constant Collation_Option; -- Ignore separators at the beginning
   Tail   : constant Collation_Option; -- Ignore separators at the end
   Middle : constant Collation_Option; -- Collate separators in the middle
   Full   : constant Collation_Option; -- Full collation

   --  Function to combine different collation options.  For example,
   --
   --          Head and Tail
   --
   --  ignores separators at the beginning and at the end, but does not
   --  do any collation in the middle, so that the splitting of
   --  ";foo;;bar;pooh;" would give
   --
   --        "foo"   ""    "bar"   and  "pooh"
   function "and" (X, Y : Collation_Option) return Collation_Option;

   function Split (To_Be_Splitted : String;
                   Separator      : Ada.Strings.Maps.Character_Set;
                   Collation      : Collation_Option) return Token_Array
     with
       Pre =>
         To_Be_Splitted'Last < Integer'Last - 2,
         Post =>
           (if To_Be_Splitted = "" then Split'Result'Length = 0),
     Annotate => (Gnatprove, Terminating);

   --
   --  Split string To_Be_Splitted in substring separated by any character in
   --  Separator.  If Collate_Separator is true consider consecutive
   --  istances of Separator as a single one.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean) return Token_Array
     with
       Pre =>
         To_Be_Splitted'Last < Integer'Last - 2,
         Post =>
           (if To_Be_Splitted = "" then Split'Result'Length = 0),
           Annotate => (Gnatprove, Terminating);

   --
   --  Synctactic sugar when only a single separator (and not a set) is
   --  used.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Character;
                   Collate_Separator : Boolean) return Token_Array
   is (Split (To_Be_Splitted    => To_Be_Splitted,
              Separator         => Ada.Strings.Maps.To_Set (Separator),
              Collate_Separator => Collate_Separator))
     with  Pre =>
       To_Be_Splitted'Last < Integer'Last - 2,
       Post =>
         (if To_Be_Splitted = "" then Split'Result'Length = 0);

   --
   --  Synctatic sugar with Separator defaulting to the space, with
   --  Collate_Separator True if Separator is the space, false otherwise
   --
   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ')
                   return Token_Array
   is (Split (To_Be_Splitted, Separator, Separator = ' '))
     with Pre => To_Be_Splitted'Last < Integer'Last - 2;

   Empty_Array : constant Token_Array (1 .. 0) :=
                   (others => Null_Unbounded_String);

private

   type Basic_Collation_Option is (Ignore_Head, Collate_Middle, Ignore_Tail);
   type Collation_Option is array (Basic_Collation_Option) of Boolean;

   None        : constant Collation_Option := (others => False);
   Head        : constant Collation_Option := (Ignore_Head => True, others => False);
   Tail        : constant Collation_Option := (Ignore_Tail => True, others => False);
   Middle      : constant Collation_Option := (Collate_Middle => True, others => False);
   Full        : constant Collation_Option := (others => True);

   overriding function "and" (X, Y : Collation_Option) return Collation_Option
   is (X or Y);

end Tokenize;
