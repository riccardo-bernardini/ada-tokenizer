#!/usr/bin/bash

gprbuild -q test_tokenizer

cases=('n%foo  bar%[foo] [] [bar]'
       'f%foo  bar%[foo] [bar]'
       'f%foo,,bar%[foo] [bar]'
       'n%foo,,bar%[foo] [] [bar]'
      )

for this in "${cases[@]}" ; do
    IFS='%' read mode input expected <<< "$this"

    output=`obj/test_tokenizer -q $mode "$input"`

    if [ "$output" = "$expected" ]; then
	echo PASSED
    else
	echo "FAILED [$mode $input/$output/$expected]"
    fi
done
