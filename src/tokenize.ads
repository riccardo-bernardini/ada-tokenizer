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
-- This package provides a function Split which divides its input
-- string in smaller strings, separated by a "separator" (much as the
-- split function in Perl, Ruby, and so on...).  Function Split returns
-- a Token_List (defined by this package)  whose elements can be accessed
-- by the function Element.
--
-- Function Split can accept a third Boolean value Collate_Separator.
-- If Collate_Separator is true, consecutive istances of the separator are
-- considered as a single one.  If Collate_Separator is False, for every
-- pair of consecutive separator characters an empty string will be returned.
-- Moreover, if Collate_Separator is True, any separator at the beginning of
-- the string is ignored.  Separators at the end are always ignored.
--
-- The default value of Collate_Separator is true if the separator
-- is the space, false otherwise.
--
-- Examples:
--
--   Split("Hello   there")               returns "Hello" and "there"
--   Split("Hello   there", ' ', False)   returns "Hello", "" and "there"
--   Split("Hello::there", ':')           returns "Hello", "" and "there"
--   Split("Hello::there", ':', True)     returns "Hello" and "there"
--
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Maps;

use Ada;

package Tokenize with SPARK_Mode => On is
   --     package String_Vectors is
   --           new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
   --                                                  Element_Type => String);

   --     subtype Token_List is String_Vectors.Vector;
   type    Token_Array is array (Positive range <>) of Unbounded_String;
   --     use type Token_List;


   --
   -- Split string To_Be_Splitted in substring separated by any character in
   -- Separator.  If Collate_Separator is true consider consecutive
   -- istances of Separator as a single one.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean) return Token_Array
     with
       Pre =>
         To_Be_Splitted'Last < Integer'Last - 2,
         Post =>
           ((To_Be_Splitted = "") <= (Split'Result'Length = 0)),
           Annotate => (Gnatprove, Terminating);
   --
   -- Synctactic sugar when only a single separator (and not a set) is
   -- used.
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
         ((To_Be_Splitted = "") <= (Split'Result'Length = 0));
   --


   --
   -- Synctatic sugar with Separator defaulting to the space, with
   -- Collate_Separator True if Separator is the space, false otherwise
   --
   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ')
                   return Token_Array
   is (Split (To_Be_Splitted, Separator, Separator = ' '))
     with Pre => To_Be_Splitted'Last < Integer'Last - 2;

   Empty_Array : constant Token_Array (1 .. 0) := (others => Null_Unbounded_String);
end Tokenize;
