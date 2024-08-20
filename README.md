[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/splitter.json)](https://alire.ada.dev/crates/splitter.html)
# Tokenizer
Small Ada package with functions "Ruby split"-like.  This has been also a "gym" to try to use SPARK.

Package `Tokenize` exports mainly a function

```Ada
function Split (To_Be_Splitted    : String;
                Separator         : Ada.Strings.Maps.Character_Set;
                Collate_Separator : Boolean) return Token_Array;
```
that splits its argument at the characters in the character set Separator. If Collate_Separator is True, then consecutive separators are considered as one.  

Another function allows for a finer control of collation

```ada
function Split (To_Be_Splitted : String;
                Separator      : Ada.Strings.Maps.Character_Set;
                Collation      : Collation_Option) return Token_Array;
```
where `Collation` can assume values like `None` (for no collation), `Head` (only at the beginning of the string), `Tail` (onluy at the end), and so on.  See the spec file for details

Some simpler functions (mostly syntactic sugar) are also provided, for example,

```Ada
function Split (To_Be_Splitted : String;
                Separator      : Character := ' ')
                return Token_Array;
```
with a behaviour Ruby-like: by default `Separator` is the space; if `Separator = ' '` then `Collate_Separator` is assumed `True`, otherwise is assumed `False`. 
