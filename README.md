# ada-tokenizer
Small Ada package with functions "Ruby split"-like.  This has been also a "gym" to try to use SPARK.

The package export mainly a function

```Ada
function Split (To_Be_Splitted    : String;
                Separator         : Ada.Strings.Maps.Character_Set;
                Collate_Separator : Boolean) return Token_Array;
```
that splits its argument at the characters in the character set Separator. If Collate_Separator is True, then consecutive separators are considered as one.  There is also a second function (mstly syntactic sugar)

```Ada
function Split (To_Be_Splitted : String;
                Separator      : Character := ' ')
                return Token_Array;
```
with a behaviour Ruby-like: by default `Separator` is the space; if `Separator = ' '` then `Collate_Separator` is assumed `True`, otherwise is assumed `False`. 
